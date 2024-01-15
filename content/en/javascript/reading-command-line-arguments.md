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

## Why
Have you ever needed to run a Javascript program from the command line and provide specific arguments? Maybe you want to customize the output of your program or provide input for it to work with. This is where reading command line arguments becomes useful.

## How To
Reading command line arguments in Javascript is actually quite simple. First, we need to use the `process.argv` method to access the arguments passed in. Let's take a look at an example:

```
// myProgram.js

const args = process.argv.slice(2); // slicing the first two arguments which are unnecessary
console.log(`Hello ${args[0]}!`); // assuming the first argument is the name passed in
```

Now, if we run this program from the command line with `node myProgram.js John`, the output will be `Hello John!`. 

We can also provide multiple arguments and access them accordingly. Here's an example:

```
// calculator.js

const args = process.argv.slice(2);
const num1 = parseInt(args[0]);
const num2 = parseInt(args[2]);
const operation = args[1];

if (operation === '+') {
  console.log(num1 + num2);
} else if (operation === '-') {
  console.log(num1 - num2);
} else if (operation === '*') {
  console.log(num1 * num2);
} else {
  console.log('Invalid operation! Please use +, -, or *');
}
```
Running `node calculator.js 5 + 3` in the command line will output `8`. Similarly, `node calculator.js 10 * 2` will output `20`.

## Deep Dive
Let's break down the `process.argv` method used in the examples above. This method returns an array that contains the command line arguments passed in. The first two elements of this array are always the `node` executable and the name of the Javascript file being executed. That's why we use the `slice()` method to remove those elements before accessing our desired arguments.

It's also worth noting that the arguments passed in are always strings, so if we need to perform any numerical operations, we have to use `parseInt()` to convert them to numbers.

## See Also
- [Node.js Documentation: Process](https://nodejs.org/api/process.html#process_process_argv)
- [FreeCodeCamp - Using command line arguments in Node.js](https://www.freecodecamp.org/news/node-js-command-line-arguments-tutorial/#:~:text=The%20standard%2C%20long%2Dwinded%20way,arguments%20in%20Node%3A%20process.)