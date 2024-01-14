---
title:    "Javascript recipe: Writing to standard error"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

#Why Writing to Standard Error is Important in Javascript Programming

When writing Javascript code, it is important to have a good understanding of how to handle errors. One way to handle errors is by writing them to the standard error output. This ensures that any potential issues are captured and can be addressed appropriately.

##How To Write to Standard Error in Javascript

Writing to standard error in Javascript is a simple process. You can use the `console.error()` method to log errors to the standard error output. For example:

```
Javascript
console.error("There was an error in your code");
```

This will print the error message to the console, allowing you to quickly identify and fix any issues in your code. The standard error output is typically displayed in red, making it easy to spot among other console logs.

You can also include variables or other information in your error message, as shown in the example below:

```
Javascript
let num1 = 10;
let num2 = "5";

if (typeof num2 !== "number") {
  console.error(`Error: ${num2} is not a number`);
}
```

This will output "Error: 5 is not a number" to the console, helping you to identify which variable caused the error and what type of error it is.

##Deep Dive into Writing to Standard Error in Javascript

When writing to standard error in Javascript, it is important to note that you can also use the `throw` keyword to throw custom errors. This allows you to create your own error messages and handle them in a specific way. For example:

```
Javascript
function divide(num1, num2) {
  if (num2 === 0) {
    throw "Cannot divide by zero";
  } else {
    return num1 / num2;
  }
}

console.log(divide(10, 0));
```

In this code, if the `num2` parameter is equal to 0, a custom error message will be thrown. This allows you to handle specific errors in a more personalized manner.

Additionally, you can use the `try...catch` statement to catch errors and handle them in a specific way. This is useful when you want to continue running your code even if an error occurs. For example:

```
Javascript
try {
  // code that might cause an error
} catch (error) {
  // handle the error
  console.error("Oops, an error occurred");
}
```

##See Also

- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/throw
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch
- https://www.digitalocean.com/community/tutorials/how-to-debug-node-js-with-the-built-in-debugger-and-chrome-developer-tools