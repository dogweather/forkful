---
title:                "TypeScript recipe: Reading command line arguments"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
Command line arguments are a crucial aspect of programming, allowing developers to pass in parameters and customize their application's behavior without needing to hardcode values. Understanding how to read command line arguments in TypeScript can greatly improve your ability to build flexible and powerful applications.

## How To
In TypeScript, we can access command line arguments through the `process` object. This object provides an array of strings, `argv`, that contains all the arguments passed in when running the program. To read these arguments, we can use a `for` loop to iterate through the array and perform any necessary logic.

```
TypeScript
for (let i = 0; i < process.argv.length; i++) {
  // do something with each argument in process.argv
}
```

Let's say we have a program that calculates the area of a rectangle using command line arguments for the length and width. We can use `process.argv[2]` and `process.argv[3]` to access these values and then convert them to numbers using `parseInt()`.

```
TypeScript
const length = parseInt(process.argv[2]);
const width = parseInt(process.argv[3]);
const area = length * width;
console.log(`The area of the rectangle is ${area} square units.`);
```

If we run our program with the command `node index.js 5 8`, our output would be `"The area of the rectangle is 40 square units."` We can also add in error handling to ensure that the correct number of arguments are provided and that they are of the correct type.

## Deep Dive
In addition to accessing command line arguments through `process.argv`, we can also use third-party libraries like `yargs` or `commander` to parse and handle command line arguments more efficiently. These libraries offer functionality such as automatically setting up help menus and validating input, making it easier for developers to build robust command line tools. It's important to note that these third-party libraries are not specific to TypeScript and can be used with any Node.js application.

## See Also
- [Node.js process.argv documentation](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [yargs code repository on GitHub](https://github.com/yargs/yargs)
- [commander code repository on GitHub](https://github.com/tj/commander.js)