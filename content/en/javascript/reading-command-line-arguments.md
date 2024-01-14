---
title:                "Javascript recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Why

Have you ever wanted to create a Javascript program that can be run through the command line? Or maybe you have come across a situation where you needed to pass specific values or options to your program before running it. In both cases, having the ability to read command line arguments can be extremely useful.

##How To

Reading command line arguments in Javascript is actually quite simple. All you need is the `process.argv` property, which is available in both Node.js and browser environments. `process.argv` is an array that stores all the arguments passed to a program through the command line.

Let's take a look at an example. Suppose we have a program called `divider.js` which takes in two numbers and divides them, returning the result. We can pass these two numbers as command line arguments when running the program:

```Javascript
// divider.js

let num1 = process.argv[2]; // first argument
let num2 = process.argv[3]; // second argument

let result = num1 / num2;

console.log("The result is: " + result);
```

If we run this program in the command line with the command `node divider.js 10 5`, the output will be `The result is: 2`. Here, `process.argv[0]` will be the path to the Node.js binary, `process.argv[1]` will be the path to the script (`divider.js`), `process.argv[2]` will be `10`, and `process.argv[3]` will be `5`.

We can also pass in multiple arguments:

```Javascript
// greetings.js

let name = process.argv[2]; // first argument
let age = process.argv[3]; // second argument

console.log("Hello " + name + "! You are " + age + " years old.");
```

Running `node greetings.js John 25` will output `Hello John! You are 25 years old.`. Here, `process.argv[0]` and `process.argv[1]` will be the same as the previous example, while `process.argv[2]` will be `John` and `process.argv[3]` will be `25`.

##Deep Dive

There are a few things to keep in mind when reading command line arguments. Firstly, all the arguments are treated as strings, so you need to convert them to the desired data type (e.g. using `parseInt()` for numbers). Secondly, if there are spaces in an argument, it needs to be surrounded by double quotes when passed in the command line (e.g. `node program.js "string with spaces"`). Finally, if you need to pass in a flag (e.g. `--verbose`), you can check for its existence by using `process.argv.includes("--verbose")` which will return a boolean value.

##See Also

Here are some helpful resources for further reading on working with command line arguments in Javascript:

- [Node.js Documentation on process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Commander.js - A popular library for building command line interfaces in Node.js](https://www.npmjs.com/package/commander)
- [Getting Started with Command Line Arguments in Javascript](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-node-js)

Now that you know how to read command line arguments, you can incorporate this knowledge into your Javascript programs and make them even more versatile. Happy coding!