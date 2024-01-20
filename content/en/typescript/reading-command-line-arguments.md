---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Reading Command Line Arguments with TypeScript

## What & Why? 
Reading command line arguments is a method that allows programmers to input data directly into their scripts via the command line interface. It's beneficial in a variety of contexts such as altering script behavior without requiring code changes, and for passing necessary variables at runtime.

## How to:

Let's take a look at how we can read command line arguments with TypeScript:

```TypeScript
let args: string[] = process.argv.slice(2);
console.log(args);
```

This script cuts off the first two arguments (node executable and script path) and returns the rest of the command line arguments. Run it with some arguments:

```bash
$ node script.ts arg1 arg2 arg3
```

The result will be:

```bash
[ 'arg1', 'arg2', 'arg3']
```

## Deep Dive

Historically, command line arguments have been used in UNIX systems, where small, concise command line utilities were common. Today, we use them to make our scripts more flexible and interactive.

While using `process.argv` is the most straightforward method of getting command line arguments in TypeScript, there are alternative packages like `yargs` or `commander` that provide advanced features like flags, defaults, and richer error messages.

Reading command line arguments in TypeScript essentially boils down to how V8 engine (which underpins Node.js) deals with them. `process.argv` is just an array reflecting command line call structure, where each space-separated part becomes an array member.

## See Also

Lightweight `minimist` npm package: https://www.npmjs.com/package/minimist

yargs npm package for more complex cases: https://www.npmjs.com/package/yargs

Node.js process.argv documentation: https://nodejs.org/docs/latest/api/process.html#process_process_argv