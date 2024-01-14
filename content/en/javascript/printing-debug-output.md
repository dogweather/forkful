---
title:                "Javascript recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why 

When working on a Javascript program, it's common to encounter errors or unexpected behavior. In order to track down and fix these issues, it can be helpful to print out debug output during the execution of your code. This allows you to see the values of variables at different points in your program and identify where things may be going wrong. 

## How To

To print debug output in Javascript, we can use the `console.log()` function. This function takes in any number of arguments and prints them to the console. Let's take a look at a simple example: 

```Javascript
let num1 = 5;
let num2 = 10;
let sum = num1 + num2;
console.log("The sum of " + num1 + " and " + num2 + " is " + sum);
```

In this code block, we declare two variables and assign them values. Then, we calculate the sum of these variables and print the result to the console. The output should look like this: 

`The sum of 5 and 10 is 15`

We can also use `console.log()` to print the value of a variable or even an entire object. Let's say we have an array of numbers and we want to see the values of each element. We can do this by passing the array to `console.log()` like this: 

```Javascript
let numbers = [1, 2, 3, 4, 5];
console.log(numbers);
```

The output will show us the contents of the array: 

`[1, 2, 3, 4, 5]`

Lastly, we can also use `console.log()` to print out messages or labels to make our debug output more organized and easy to read. Take a look at this example: 

```Javascript
let name = "John";
let age = 25;
console.log("Name: " + name);
console.log("Age: " + age + " years old");
```

This will output: 

```
Name: John
Age: 25 years old
```

## Deep Dive

The `console.log()` function is just one of the ways to print debug output in Javascript. Another option is to use the `debugger` statement, which will pause the execution of your code and allow you to step through it line by line. This can be helpful for more complex programs where `console.log()` may not provide enough information. 

It's also important to note that printing debug output can slow down the execution of your code, so it's best to use it only when needed and remove it before deploying your program. There are also more advanced ways of debugging in Javascript, such as using a debugger tool in your browser's developer tools. 

## See Also 

Check out these helpful resources to learn more about debugging in Javascript:

- [Debugging JavaScript with the Console API](https://www.digitalocean.com/community/tutorials/how-to-debug-javascript-with-the-console-api)
- [A Beginner's Guide to Debugging JavaScript in Chrome DevTools](https://www.freecodecamp.org/news/a-beginners-guide-to-debugging-javascript-in-chrome-devtools-2e6626c5ef9e/)
- [Mastering The Debugging Tools In Javascript](https://www.smashingmagazine.com/2018/10/mastering-javascript-debugger/)