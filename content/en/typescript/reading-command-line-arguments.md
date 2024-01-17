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

## What & Why?

Reading command line arguments is the process of retrieving data or parameters that are passed to a program through the command line interface. Programmers do this in order to make their programs more flexible and customizable by allowing users to input specific values when running the program.

## How to:

In TypeScript, command line arguments can be read using the `process.argv` array. Its first two elements are reserved for the program execution path and command used, so the actual arguments start at index 2. To access these arguments, we can simply loop through the array starting at index 2 and print out each argument. For example:

```TypeScript
for (let i = 2; i < process.argv.length; i++) {
  console.log(`Argument ${i - 1}: ${process.argv[i]}`);
}
```

Running the program with the command `node index.ts argument1 argument2` would result in the output:
```
Argument 1: argument1
Argument 2: argument2
```

Alternatively, we can use the `yargs` package to make command line argument parsing even easier. This package allows us to define options and flags for our program and automatically parse the arguments for us. For example:

```TypeScript
const argv = require('yargs').argv;

console.log(`Name: ${argv.name}`);
console.log(`Age: ${argv.age}`);
```

Running the program with the command `node index.ts --name John --age 25` would result in the output:
```
Name: John
Age: 25
```

## Deep Dive:

Reading command line arguments has been a common practice since the early days of programming, when command line interfaces were the main form of user interaction with computers. It provides a convenient way for programs to receive input from users without the need for a graphical user interface. In addition to using the `process.argv` array, some other popular packages for parsing command line arguments in TypeScript include `commander` and `argparse`.

It is worth noting that command line arguments are not the only way to make programs customizable. Other methods include config files, environment variables, and interactive prompts. However, command line arguments are often preferred for their ease of use and simplicity.

## See Also:

- [process.argv documentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [yargs package](https://www.npmjs.com/package/yargs)
- [commander package](https://www.npmjs.com/package/commander)
- [argparse package](https://www.npmjs.com/package/argparse)