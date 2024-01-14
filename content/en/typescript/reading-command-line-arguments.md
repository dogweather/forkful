---
title:    "TypeScript recipe: Reading command line arguments"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

As a TypeScript programmer, you may have encountered situations where you need to pass in command line arguments to your program. Whether it's to provide user input or to customize the behavior of the program, knowing how to read command line arguments can be a useful skill to have. In this blog post, we will explore the basics of reading command line arguments in TypeScript and how you can use them in your own projects.

## How To

Reading command line arguments in TypeScript is a fairly straightforward process. The `process.argv` function is the key to retrieving and parsing the arguments. Let's take a look at a simple example where we want to print out a message based on a user's input:

```TypeScript
// printing-message.ts
const args = process.argv; // retrieving arguments
const userInput = args[2]; // accessing the third argument - first two are reserved for Node and the file path

console.log(`Hello ${userInput}! Welcome to my program.`);
```

If we run this program with the command `ts-node printing-message.ts John`, our output would be:

```
Hello John! Welcome to my program.
```

We can also pass in multiple arguments and access them accordingly. Let's modify our program to accept two user inputs:

```TypeScript
// printing-message.ts
const args = process.argv;
const firstInput = args[2];
const lastInput = args[3];

console.log(`Hello ${firstInput} ${lastInput}! Welcome to my program.`);
```

Running this program with the command `ts-node printing-message.ts John Smith` would give us:

```
Hello John Smith! Welcome to my program.
```

## Deep Dive

Now that we have the basics of reading command line arguments down, let's dive a bit deeper into the details. Firstly, it's important to note that `process.argv` returns an array of strings, with the first two elements being reserved for Node and the file path, respectively. This means that the actual user inputs start from the third element.

Additionally, all command line arguments are treated as strings, even if they are numbers. So, if you want to use a command line argument as a number in your program, you will need to convert it to a number using `parseInt()` or `parseFloat()`.

It's also worth mentioning that we can use named arguments by specifying them in the command line with a double dash, like `--input=name`. These named arguments can be accessed using the `process.argv` array with `process.argv[index].slice(2)`.

## See Also
- [Node.js process.argv documentation](https://nodejs.org/docs/latest-v8.x/api/process.html#process_process_argv)
- [TypeScript String interpolation](https://www.typescriptlang.org/docs/handbook/basic-types.html#string-interpolation)
- [TypeScript parseInt() documentation](https://www.typescriptlang.org/docs/handbook/variable-declarations.html#union-types)

With this knowledge, you should now be able to utilize command line arguments in your TypeScript programs. Happy coding!