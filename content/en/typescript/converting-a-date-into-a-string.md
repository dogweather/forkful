---
title:                "TypeScript recipe: Converting a date into a string"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a common task in programming, especially when working with date-related data. By converting a date into a string, we can easily display it in a readable format, manipulate it, or use it in other calculations or comparisons. In this blog post, we will explore how to convert a date into a string using TypeScript.

## How To

Converting a date into a string in TypeScript is a simple process. Let's take a look at an example:

```TypeScript
// Create a new Date object
const date = new Date();

// Convert the date into a string using the toDateString() method
const dateString = date.toDateString();

// Output the resulting string
console.log(dateString); // Output: Mon Jul 26 2021
```

In the above code, we first create a new Date object using the `new Date()` constructor. This gives us the current date and time. Next, we use the `toDateString()` method to convert the date into a string in the format of `Day Month Date Year`. Finally, we use the `console.log()` method to output the resulting string.

You can also customize the format of the date string by using the `toLocaleDateString()` method. This method takes in parameters for the locale and options. Let's see an example:

```TypeScript
// Create a new Date object
const date = new Date();

// Customize the date string format
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
const dateString = date.toLocaleDateString('en-US', options);

// Output the resulting string
console.log(dateString); // Output: Monday, July 26, 2021
```

Here, we passed in the American English locale and also specified the options for the output format. This gives us a more readable date string.

## Deep Dive

Behind the scenes, what's happening when we convert a date into a string is that the `toString()` method of the Date object is being called. This method returns a string representation of the date in the default format (`Day Month Date Year Time Zone`).

It's important to note that the resulting date string can vary based on the locale and options used. This is because different countries may have different date and time formats.

Additionally, the `toLocaleDateString()` method is part of the Internationalization API, which allows for localization of date and time formats. This API is supported by most modern browsers, but may not be available in older browsers. In those cases, the default format of `Day Month Date Year` will be used.

## See Also

- [Date - TypeScript Documentation](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)
- [Intl.DateTimeFormat - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat)
- [Formatting Date and Time in JavaScript - DigitalOcean Tutorial](https://www.digitalocean.com/community/tutorials/formatting-dates-and-times-in-javascript)