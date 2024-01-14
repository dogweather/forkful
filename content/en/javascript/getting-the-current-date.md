---
title:    "Javascript recipe: Getting the current date"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why
Have you ever wanted to display the current date on your website or application? Or track when a certain event happened? Knowing how to get the current date using Javascript can come in handy in many programming scenarios. In this blog post, we will explore different ways to get the current date using Javascript and the various options available to customize the date format.

## How To
Getting the current date in Javascript is simple and can be done using the `Date` object. Let's take a look at a basic example:

```Javascript
// Create a new Date object
const currentDate = new Date();

// Get the current date and time in ISO format
const isoDate = currentDate.toISOString();
console.log(isoDate); // Output: "2021-10-07T16:47:23.972Z"

// Get the current date and time in UTC format
const utcDate = currentDate.toUTCString();
console.log(utcDate); // Output: "Thu, 07 Oct 2021 16:47:23 GMT"

// Get the current date in a custom format
const day = currentDate.getDate();
const month = currentDate.getMonth() + 1; // The month starts at index 0, so we need to add 1
const year = currentDate.getFullYear();

const customFormat = `${month}/${day}/${year}`;
console.log(customFormat); // Output: "10/07/2021"
```

As you can see, the `Date` object has multiple methods that allow us to retrieve the current date and time in various formats. You can also customize the format by accessing individual components of the date, such as day, month, year, etc.

## Deep Dive
There are several things to keep in mind when working with the `Date` object in Javascript. First, the date and time displayed are based on the user's local time zone. This means that the date and time may differ from user to user. To ensure consistency, you can use the `Date.UTC()` method, which returns the current date and time in UTC format.

Another important thing to note is that the `Date` object also has methods that allow you to adjust the date and time, such as setting a specific date or time, adding or subtracting days, etc.

Moreover, if you need to compare dates, it is recommended to use timestamps instead of date objects. This can be achieved by getting the time in milliseconds using the `getTime()` method.

To learn more about the `Date` object and its various methods, refer to the official documentation [here](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date).

## See Also
- [JavaScript Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Working with Dates in JavaScript](https://www.w3schools.com/js/js_date_methods.asp)
- [Date and time in JS: Learn how to work with Date and Time in JavaScript](https://www.educative.io/blog/date-and-time-in-javascript)