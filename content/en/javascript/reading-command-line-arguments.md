---
title:                "Javascript recipe: Reading command line arguments"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

As a Javascript programmer, you may have come across the term "command line arguments" before. But why exactly would you need to read these arguments in your code? The answer is simple - command line arguments allow you to pass specific inputs to your program when it is executed. This can be useful for customizing your program's behavior or handling different scenarios. In this blog post, we'll explore how to read command line arguments in Javascript and how it can benefit your programming.

## How To

Reading command line arguments in Javascript can be done using the `process.argv` property. This property is an array that contains all the arguments passed to your program in the command line. Let's take a look at an example:

```Javascript
// Our program to read and display command line arguments
console.log(process.argv)

// Output when executing `node index.js firstArg secondArg`
// [ 'node', '/path/to/file/index.js', 'firstArg', 'secondArg' ]
```

As you can see, the first two elements of the `process.argv` array are the path to the node executable and the path to the current file. The remaining elements are the arguments passed in the command line, in the order they were entered. You can access these arguments using array indexing, starting from the third element. Let's see another example where we pass in a number as an argument and perform some operations on it:

```Javascript
// Our program to read and manipulate command line arguments
const num = parseInt(process.argv[2]) // converting argument from string to integer
const square = num * num // calculating the square
console.log(`The square of ${num} is ${square}`) // displaying the result

// Output when executing `node index.js 5`
// The square of 5 is 25
```

You can even pass in multiple arguments and make use of conditional statements to handle different scenarios. The possibilities are endless! Experiment with different inputs and see what you can come up with.

## Deep Dive

Apart from using `process.argv`, there are other methods to read command line arguments in Javascript. One of them is using a popular library called `yargs`. This library provides a more robust and user-friendly way to handle command line arguments by allowing you to specify options and flags for your program.

You can also validate the arguments passed using `process.argv` or `yargs` by checking their data types and values. This can help prevent unexpected errors and improve the overall stability of your program.

## See Also

- [Official Node.js documentation on process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Yargs documentation](https://www.npmjs.com/package/yargs)