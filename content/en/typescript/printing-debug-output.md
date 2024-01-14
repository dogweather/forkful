---
title:                "TypeScript recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of any programming process. As developers, we often encounter bugs and errors in our code that can be difficult to identify and fix. This is where printing debug output comes in handy. By printing out specific values at certain points in our code, we can better understand what is happening and troubleshoot any issues that may arise.

## How To

To print out debug output in TypeScript, we can use the `console.log()` method. This method takes in a value or variable and prints it to the console. Let's say we have a variable called `name` that stores a string value. We can print out the value of this variable by using `console.log()` like this:

```TypeScript
let name = "John";
console.log(name); // Output: John
```

We can also print out multiple values by separating them with commas inside the `console.log()` method. For example:

```TypeScript
let num1 = 10;
let num2 = 20;
console.log(num1, num2);// Output: 10 20
```

We can even use string concatenation or template literals to print out more complex debug output:

```TypeScript
let firstName = "John";
let lastName = "Doe";
console.log("Full Name: " + firstName + " " + lastName); // Output: Full Name: John Doe

// Using template literals:
console.log(`First Name: ${firstName}, Last Name: ${lastName}`); // Output: First Name: John, Last Name: Doe
```

Another useful method for printing debug output is `console.dir()`. This method allows us to view the properties and methods of an object in more detail. For example:

```TypeScript
let person = { name: "John", age: 30, occupation: "Developer" };
console.dir(person); // Output: { name: "John", age: 30, occupation: "Developer" }
```

## Deep Dive

Printing debug output is not just limited to strings and numbers. We can also use it with conditional statements and loops to check the flow of our code. For example, if we have a function that checks if a given number is even or odd, we can print out a message to indicate which condition is met:

```TypeScript
function checkEvenOrOdd(num: number) {
  if (num % 2 === 0) {
    console.log(`${num} is even.`);
  } 
  else {
    console.log(`${num} is odd.`);
  }
}
checkEvenOrOdd(4); // Output: 4 is even.
checkEvenOrOdd(5); // Output: 5 is odd.
```

We can also use `console.log()` within a loop to track the values of variables at each iteration:

```TypeScript
for (let i = 1; i <= 5; i++) {
  console.log("Counter: " + i); // Output: Counter: 1, Counter: 2, Counter: 3, Counter: 4, Counter: 5
}
```

## See Also

To learn more about debugging in TypeScript, check out the following resources:

- [Official TypeScript Documentation](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [Debugging with Visual Studio Code](https://code.visualstudio.com/docs/editor/debugging)
- [TypeScript Debugging in Chrome DevTools](https://devblogs.microsoft.com/typescript/typescript-debugging-in-chrome-devtools/)