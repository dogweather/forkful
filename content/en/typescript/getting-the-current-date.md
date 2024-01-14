---
title:    "TypeScript recipe: Getting the current date"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why 

In the world of programming, dates and time are an essential part of many applications. Whether it's for scheduling, storing data, or displaying relevant information, being able to access the current date is a common task for developers. In this blog post, we will explore how to get the current date in TypeScript and why it's a crucial skill to have when working with dates in your code.

## How To 

The process of getting the current date in TypeScript involves using the built-in Date object. This object represents a specific moment in time and allows us to access various date and time methods. Let's take a look at the following code snippet to see how we can get the current date:

```TypeScript 
// Create a new Date object 
const currentDate = new Date();

// Accessing individual components of the date 
const year = currentDate.getFullYear();
const month = currentDate.getMonth() + 1; // Note: months are zero-based 
const day = currentDate.getDate();

// Output the current date in YYYY-MM-DD format 
console.log(`${year}-${month}-${day}`); // Output: 2021-07-02 
```

In the first line, we create a new Date object, which by default represents the current date and time. We then use the Date methods like `getFullYear()`, `getMonth()`, and `getDate()` to access the current year, month, and day. It's important to note that the `getMonth()` method returns a number between 0-11, where 0 represents January, 1 represents February, and so on. Hence, we add 1 to the result to get the actual month. Lastly, we use string interpolation to format the output in YYYY-MM-DD format.

Another way to get the current date in TypeScript is to use the `toLocaleString()` method, which allows us to specify the date format to use. Let's see an example below:

```TypeScript 
// Get current date in English format 
const currentDate = new Date().toLocaleString('en-US');
console.log(currentDate); // Output: 7/2/2021, 11:38:34 AM 

// Get current date in German format 
const currentDate = new Date().toLocaleString('de-DE');
console.log(currentDate); // Output: 2.7.2021, 11:38:34
```

As you can see, the `toLocaleString()` method provides a more customizable way to get the current date according to different locales.

## Deep Dive 

The Date object in TypeScript is based on the ECMAScript standard, which defines a platform-independent date and time method. This means that the Date object is not limited to a specific operating system or environment and can be used in any TypeScript project. The Date object also allows us to perform various operations on dates, such as adding or subtracting days, hours, minutes, etc.

Moreover, the Date object has several other useful methods, such as `getTime()`, which returns the number of milliseconds since January 1, 1970, and `toISOString()`, which returns the date in ISO format. These methods can be handy when working with APIs or storing dates in databases.

## See Also 

- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Official Website](https://www.typescriptlang.org/)
- [ECMAScript Specification](https://www.ecma-international.org/publications/standards/Ecma-262.htm)

In conclusion, getting the current date in TypeScript is a straightforward process, but it's important to understand the Date object and its methods to use them effectively in your code. With this knowledge, you can now confidently handle dates and time in your TypeScript projects. Good luck and happy coding!