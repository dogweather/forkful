---
title:    "TypeScript recipe: Printing debug output"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

As a TypeScript programmer, you may have encountered situations where your code doesn't run as expected and you need to understand what's happening behind the scenes. In such cases, it can be helpful to print out debug output to better understand the flow of your program. 

## How To 

To print debug output in TypeScript, we can use the console.log() function. This function outputs the given value to the console, making it easy for us to see what's happening in our code. Let's take a look at an example: 

```TypeScript
let num1 = 10;
let num2 = 5;
console.log(num1 + num2);

// Output: 15
```

In this example, we have two variables declared and assigned values. By using console.log(), we can see the result of adding the two numbers in the output. 

We can also print out the value of a variable or an object using the same function. Let's see another example: 

```TypeScript
let str = "Hello World";
console.log(str);

// Output: Hello World
```

Here, we have a string variable and we print its value using console.log(). 

## Deep Dive 

Console.log() is not the only way to print out debug output in TypeScript. We can also use the console.dir() function to see the structure of an object in a more detailed way. Let's take a look at an example: 

```TypeScript
let user = {
  name: "John Doe",
  age: 25,
  hobbies: ["reading", "playing guitar", "hiking"]
}

console.dir(user);

/* Output:
{ 
  name: "John Doe",
  age: 25,
  hobbies: ["reading", "playing guitar", "hiking"]
}
*/
```

In this example, we print out the user object using console.dir() and we can see the properties and values of the object in a more organized way.

It's important to remember to remove any debug output before publishing your code or sharing it with others, as it can clutter the console and affect the performance of your application.

## See Also 

For more information on debugging in TypeScript, you can check out the following resources:

- [TypeScript debugging tutorial](https://code.visualstudio.com/docs/nodejs/nodejs-debugging)
- [Mastering TypeScript debugging](https://blog.logrocket.com/mastering-typescript-debugging-how-to-start-tracking-down-errors-right-now/)
- [Debugging with console.log() in TypeScript](https://medium.com/@hew/how-to-debug-in-typescript-fb6dcff58614)

Happy coding!