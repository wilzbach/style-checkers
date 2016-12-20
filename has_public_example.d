#!/usr/bin/env dub
/+ dub.sdl:
name "test_validator"
dependency "libdparse" version="~>0.7.0-beta.2"
+/
/*
 * Checks that all functions have a public example
 *
 * Copyright (C) 2016 by D Language Foundation
 *
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
*/
// Written in the D programming language.

import dparse.ast;
import std.algorithm;
import std.experimental.logger;
import std.range;
import std.stdio;

bool hadError;

class TestVisitor : ASTVisitor
{

    this(string fileName, ubyte[] sourceCode)
    {
		this.fileName = fileName;
        this.sourceCode = sourceCode;
    }

    alias visit = ASTVisitor.visit;

	override void visit(const Module mod)
	{
		FunctionDeclaration lastFun;
		bool hasPublicUnittest;

		foreach (decl; mod.declarations)
		{
			if (!isPublic(decl.attributes))
				continue;

			if (decl.functionDeclaration !is null)
			{
				if (hasDitto(decl.functionDeclaration))
					continue;

				if (lastFun !is null && !hasPublicUnittest)
					triggerError(lastFun);

				lastFun = cast(FunctionDeclaration) decl.functionDeclaration;
				hasPublicUnittest = false;
			}

			if (decl.unittest_ !is null)
				hasPublicUnittest |= validate(lastFun, decl.unittest_);

		}

		if (lastFun !is null && !hasPublicUnittest)
			triggerError(lastFun);
	}

private:
	string fileName;
    ubyte[] sourceCode;

	void triggerError(const FunctionDeclaration decl)
	{
		stderr.writefln("%s:%d has no public unittest", fileName, decl.name.line);
		hadError = true;
	}

	bool validate(const FunctionDeclaration decl, const Unittest u)
	{
		// ignore module header unittest blocks or already validated functions
		if (decl is null)
			return true;

		if (!hasDdocHeader(u))
			return false;

		return true;
	}

	bool hasDdocHeader(const Unittest u)
	{
        // scan the previous line for ddoc header
        auto prevLine = sourceCode[0 .. u.location].retro;
        prevLine.findSkip("\n"); // skip forward to the previous line
        auto ddocCommentSlashes = prevLine.until('\n').count('/');

        // only look for comments annotated with three slashes (///)
        if (ddocCommentSlashes == 3)
            return true;

        if (u.comment !is null)
        {
            prevLine = sourceCode[0 .. u.location].retro;
            prevLine.findSkip("\n"); // skip forward to the previous line
            // detect other common comment forms - be careful in reverse form
            if (prevLine.until('\n').equal("/*"))
                return true;
            if (prevLine.until('\n').equal("/+"))
                return true;
        }

		return false;
	}

	bool hasDitto(const FunctionDeclaration decl)
	{
		if (decl.comment is null)
			return false;

		if (decl.comment == "ditto")
			return true;

		if (decl.comment == "Ditto")
			return true;

		return false;
	}

	bool isPublic(const Attribute[] attrs)
	{
		import dparse.lexer : tok;
		import std.algorithm.searching : any;
		import std.algorithm.iteration : map;

		enum tokPrivate = tok!"private", tokProtected = tok!"protected", tokPackage = tok!"package";

		if (attrs.length > 0)
			if (attrs.map!`a.attribute`.any!(x => x == tokPrivate || x == tokProtected || x == tokPackage))
				return false;

		return true;
	}
}

void parseTests(string fileName)
{
    import dparse.lexer;
    import dparse.parser;
    import dparse.rollback_allocator;
    import std.array : uninitializedArray;
	import std.conv : to;
	import std.file : exists;

    assert(exists(fileName));

    File f = File(fileName);

    if (f.size == 0)
    {
        warningf("%s is empty", fileName);
        return;
    }

    ubyte[] sourceCode = uninitializedArray!(ubyte[])(to!size_t(f.size));
    f.rawRead(sourceCode);
    LexerConfig config;
    StringCache cache = StringCache(StringCache.defaultBucketCount);
    auto tokens = getTokensForParser(sourceCode, config, &cache);
    RollbackAllocator rba;
    Module m = parseModule(tokens.array, fileName, &rba);
    auto visitor = new TestVisitor(fileName, sourceCode);
    visitor.visit(m);
}

void parseFile(string inputDir, string fileName)
{
    import std.path : buildPath, dirSeparator, buildNormalizedPath;

    // file name without its parent directory, e.g. std/uni.d
    string fileNameNormalized = (inputDir == "." ? fileName : fileName.replace(inputDir, ""));

    // remove leading dots or slashes
    while (!fileNameNormalized.empty && fileNameNormalized[0] == '.')
        fileNameNormalized = fileNameNormalized[1 .. $];
    if (fileNameNormalized.length >= dirSeparator.length &&
            fileNameNormalized[0 .. dirSeparator.length] == dirSeparator)
        fileNameNormalized = fileNameNormalized[dirSeparator.length .. $];

    // convert the file path to a nice output file, e.g. std/uni.d -> std_uni.d
    string outName = fileNameNormalized.replace(dirSeparator, "_");

    parseTests(fileName);
}

void main(string[] args)
{
	import std.file;
    import std.getopt;
	import std.path : asNormalizedPath;

    string inputDir;
    string ignoredFilesStr;

    auto helpInfo = getopt(args, config.required,
            "inputdir|i", "Folder to start the recursive search for unittest blocks (can be a single file)", &inputDir,
            "ignore", "Comma-separated list of files to exclude (partial matching is supported)", &ignoredFilesStr);

    if (helpInfo.helpWanted)
    {
        return defaultGetoptPrinter(`example_validator
Searches the input directory recursively to ensure that all public functions
have a public unittest blocks, i.e.
unittest blocks that are annotated with three slashes (///).
`, helpInfo.options);
    }

    inputDir = inputDir.asNormalizedPath.array;

    DirEntry[] files;

    if (inputDir.isFile)
    {
        files = [DirEntry(inputDir)];
        inputDir = ".";
    }
    else
    {
        files = dirEntries(inputDir, SpanMode.depth).filter!(
                a => a.name.endsWith(".d") && !a.name.canFind(".git")).array;
    }

    auto ignoringFiles = ignoredFilesStr.split(",");

    foreach (file; files)
        if (!ignoringFiles.any!(x => file.name.canFind(x)))
            parseFile(inputDir, file);

	import core.stdc.stdlib : exit;
	if (hadError)
		exit(1);
}
