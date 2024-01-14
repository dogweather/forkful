---
title:    "TypeScript recipe: Converting a date into a string"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why
As programmers, we often need to work with dates and times in our applications. And at times, we might need to convert these dates into strings for various reasons such as displaying them in a user-friendly format or storing them in a database. In this blog post, we'll explore how to convert a date into a string using TypeScript.

## How To
To convert a date into a string in TypeScript, we can use the `toLocaleDateString()` method. This method formats a Date object into a string using the system's local time zone, language, and conventions. Let's take a look at an example:

```TypeScript
// Create a new Date object
const today = new Date();

// Use toLocaleDateString() to convert it into a string
const stringDate = today.toLocaleDateString();

// Output the string date
console.log(stringDate); // Output: 7/27/2021
```

In the above example, we first created a new Date object using the `new` keyword. Then, we used the `toLocaleDateString()` method to convert it into a string and stored the result in a variable called `stringDate`. Finally, we outputted the string date using `console.log()`. As you can see, the output is in the format of month/day/year.

However, the `toLocaleDateString()` method also accepts two optional parameters, `locales` and `options`, that allow us to specify the language and formatting options for the string date. Let's take a look at an example:

```TypeScript
// Create a new Date object
const today = new Date();

// Specify locale and options
const stringDate = today.toLocaleDateString("en-GB", { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' });

// Output the string date
console.log(stringDate); // Output: Tuesday, 27 July 2021
```

In this example, we used `'en-GB'` as the `locale` parameter to output the date in British English format, and we also specified the `options` parameter to include the weekday and month in the output. Play around with different locales and options to get the desired string date format.

## Deep Dive
The `toLocaleDateString()` method uses the system's default locale and options if none are provided. It also allows us to pass in an array of locales and will return a string using the first one that is supported by the system. This can be useful for internationalization purposes, as it allows us to display dates in different languages and formats to suit our users' preferences.

It's also important to note that the `toLocaleDateString()` method does not directly change the Date object, but rather returns a string representation of it. So we can still use the original Date object for other operations.

## See Also 
- [MDN Web Docs | Date.prototype.toLocaleDateString()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [TypeScript | Date](https://www.typescriptlang.org/docs/handbook/date-and-time.html)

I hope this blog post helped you understand the basics of converting a date into a string using TypeScript. Give it a try in your own projects and see how you can use it to improve the user experience. Happy coding!