---
title:                "Javascript recipe: Printing debug output"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

As programmers, one of the most important tasks we have is debugging our code. It's the process of finding and fixing errors in our code to ensure that it functions as intended. However, debugging can be a challenging task, especially when working with complex code. That's where printing debug output comes in.

## Why

Printing debug output is a helpful technique that allows us to see what's happening in our code as it runs. It displays specific information such as variable values, function calls, and error messages, helping us track down and fix bugs more efficiently. Without debug output, we would have to rely on trial and error or constantly check each line of code, which can be time-consuming and frustrating.

## How To

Printing debug output is a straightforward process in Javascript. The console.log() function is commonly used to print debug output. Let's take a look at an example:

```Javascript
let num1 = 5;
let num2 = 10;
let result = num1 + num2;
console.log(result);
```
Output: 15

In the above code, we first declare two variables, num1 and num2, and assign them values of 5 and 10 respectively. Then, we use the addition operator to assign the result of num1 + num2 to the variable result. Finally, we print the value of result using console.log(). This is a simple example, but it shows how we can use debug output to see the value of variables at different points in our code.

We can also use console.log() to print strings and objects. For example:

```Javascript
let name = "John";
console.log("Hello " + name);
```
Output: Hello John

```Javascript
let person = {
  name: "Jane",
  age: 25,
  profession: "Software Developer"
};
console.log(person);
```
Output: { name: 'Jane', age: 25, profession: 'Software Developer' }

## Deep Dive

In addition to using console.log(), there are other methods for printing debug output in Javascript. The console.debug() function is similar to console.log() but is intended for debugging purposes and may include additional information. We can also use console.table() to display data in a tabular format.

In some cases, we may want to print debug output only when certain conditions are met. This is where console.assert() comes in. We can pass a condition and a message to console.assert(), and if the condition is not met, the message will be printed to the console.

Debugging can also become more efficient by using a logging library or debugger tool. These tools offer more features and a better organized way of printing debug output.

## See Also

Here are some useful resources for learning more about printing debug output in Javascript:

- [MDN Web Docs: console.log()](https://developer.mozilla.org/en-US/docs/Web/API/Console/log)
- [MDN Web Docs: console.debug()](https://developer.mozilla.org/en-US/docs/Web/API/Console/debug)
- [MDN Web Docs: JavaScript debugging for beginners](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/First_steps/Debugging)
- [Chrome DevTools: Debugging JavaScript](https://developers.google.com/web/tools/chrome-devtools/javascript)