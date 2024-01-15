---
title:                "Reading command line arguments"
html_title:           "TypeScript recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're a developer working with TypeScript, understanding how to read and use command line arguments can greatly enhance your workflow and make your code more efficient. This skill can also be useful for troubleshooting and debugging purposes.

## How To

Reading command line arguments in TypeScript is a simple and straightforward process. First, we need to import the built-in "process" module, which provides access to the command line arguments. Then, we can access the arguments using the "argv" property, which is an array containing all the arguments passed in when the program was executed.

Let's take a look at a simple example:

```typescript
// Import the process module
import * as process from 'process';

// Access the command line arguments using 'argv'
const arguments = process.argv;

// Print out the arguments array
console.log(arguments);
```

If you run this program in your command line with some arguments, you will see the arguments array printed out. For example:

```
$ ts-node index.ts arg1 arg2

// Output:
[ 'node', '/path/to/your/file/index.ts', 'arg1', 'arg2' ]
```

As you can see, the first two items in the array are 'node' and the path to your file, followed by the arguments that you provided.

You can also access individual arguments by index, starting from 2 (since the first two items are not part of the arguments provided):

```typescript
// Accessing the first argument
const firstArg = process.argv[2];
console.log(firstArg);

// Output:
arg1
```

You can also use command line flags with arguments. These are usually preceded by a '-' or '--'. Let's take a look at an example:

```
$ ts-node index.ts --env development

// Output:
[ 'node', '/path/to/your/file/index.ts', '--env', 'development' ]
```

As you can see, the '--env' flag is treated as a separate argument and can be accessed in the same way as the regular arguments.

## Deep Dive

In addition to accessing the arguments themselves, the "process" module also provides some useful information about the execution environment. For example, you can access the current working directory with the "cwd()" method, or the name of the script with the "title" property.

You can also use third-party libraries like "yargs" to parse and validate the command line arguments. This can be particularly useful for more complex programs with multiple command line options and flags.

## See Also

- [Node.js process module documentation](https://nodejs.org/dist/latest-v16.x/docs/api/process.html)
- [yargs documentation](https://www.npmjs.com/package/yargs)