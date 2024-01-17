---
title:                "Reading command line arguments"
html_title:           "Javascript recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Reading command line arguments is a way for programmers to pass information or settings to their program through the command line interface instead of within the code itself. This allows for more flexibility and customization without having to constantly modify and recompile the code. 

## How to:
To read command line arguments in Javascript, we can use the global `process` object that is available in Node.js. The `process` object has an array called `argv` which contains all the command line arguments passed to the program.
```Javascript
const args = process.argv;
console.log(args);
```
Running the code above with the command `node index.js arg1 arg2` would output `[ 'node', 'index.js', 'arg1', 'arg2' ]`.

We can also use destructuring assignment to assign specific arguments to variables. For example:
```Javascript
const [,, arg1, arg2] = process.argv;
console.log(`The first argument is ${arg1} and the second argument is ${arg2}.`);
```
Running the code above with the same command as before would output `The first argument is arg1 and the second argument is arg2.`

## Deep Dive:
The concept of command line arguments has been around since the early days of computing, where programs were run through a command-line interface rather than a graphical user interface. It has been a common practice in many programming languages, including C, Java, and Perl.

In addition to using the `process.argv` array, there are other packages available such as `yargs` and `commander` that make it easier to handle command line arguments and provide features such as flags and options.

When reading command line arguments, it's important to take into consideration error handling and validation to ensure that the correct type and number of arguments are provided. Otherwise, the program may crash or produce unexpected results.

## See Also:
- [Node.js Documentation for Command Line Arguments] (https://nodejs.org/dist/latest-v14.x/docs/api/process.html#process_process_argv)
- [Command Line Arguments in C] (https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Using the yargs Package in Node.js] (https://www.npmjs.com/package/yargs)