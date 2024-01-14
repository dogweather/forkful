---
title:                "TypeScript recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Have you ever wondered how programmers are able to read command line arguments and use them to modify their code's behavior? Reading command line arguments is an essential skill for any programmer, as it allows for added functionality and customization in their programs. In this blog post, we'll explore how to read command line arguments in TypeScript and why it's a crucial skill for programmers.

## How To
To read command line arguments in TypeScript, we can use the `process.argv` property. This property returns an array of strings, where the first element is the path to the TypeScript file being executed, and the rest are the command line arguments passed in.

```TypeScript
// Simple example
console.log(process.argv);

// Output: ['path/to/file.ts', 'arg1', 'arg2', 'arg3']
```

We can also use the `slice()` method to remove the first element (the file path) and access only the command line arguments passed in.

```TypeScript
// Slicing the array to remove the first element
const args = process.argv.slice(2);

// Parsing arguments as integers
const num1 = parseInt(args[0]);
const num2 = parseInt(args[1]);

// Performing some calculation
const result = num1 + num2;

// Output: 10 (if args[0] = '5' and args[1] = '5')
console.log(result);
```

We can also use the `find()` method to search for specific arguments and take different actions based on their presence.

```TypeScript
// Filtering arguments to find a specific one
const arg = process.argv.find(arg => arg === '--verbose');

// Checking if '--verbose' arg is present
if (arg) {
  // Run code with extra logging
  console.log('Verbose mode enabled!');
} else {
  // Run code without extra logging
  console.log('Verbose mode disabled!');
}
```

## Deep Dive
In addition to the methods mentioned above, there are a few things to keep in mind when working with command line arguments in TypeScript.

1. The arguments are always strings, so we need to use methods like `parseInt()` or `Number()` to convert them into numbers if needed.
2. We can also access specific arguments using their index, for example, `process.argv[2]` would get the first argument passed in.
3. Arguments are separated by spaces, so if you need to pass a string as an argument, make sure to wrap it in quotes in the command line.

## See Also
- [TypeScript Official Documentation on Command Line Arguments](https://www.typescriptlang.org/docs/handbook/advanced-types.html#handling-different-types)
- [Node.js Documentation on Process Object](https://nodejs.org/dist/latest-v14.x/docs/api/process.html#process_process_argv)
- [MDN Web Docs on String Methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#methods)