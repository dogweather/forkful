---
title:                "Javascript recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the current date for a project? Maybe you're building a to-do app and want to display the date for each task, or you need to track when a certain task was completed. Whatever the reason, getting the current date is a basic but essential task in programming.

## How To

Getting the current date in Javascript is actually quite simple. There is a built-in Date object that we can use to access the current date and time. Here's an example:

```Javascript
// create a new Date object
let currentDate = new Date();
// get the current date and time
console.log(currentDate);
// output: Sat Aug 07 2021 09:00:00 GMT+0000 (Coordinated Universal Time)
```

In the example above, we created a new Date object called `currentDate` and then used the `console.log()` function to output the current date and time. The output will vary depending on your current timezone, but it will follow the same format.

What if we want to display the current date in a specific format, such as "DD/MM/YYYY"? We can use the `getDate()`, `getMonth()`, and `getFullYear()` methods to access and format the specific parts of the date. Here's an example:

```Javascript
// create new Date object
let currentDate = new Date();
// get date, month, and year
let date = currentDate.getDate();
let month = currentDate.getMonth() + 1; // +1 because getMonth() starts at 0 for January
let year = currentDate.getFullYear();
// format the date as DD/MM/YYYY
let formattedDate = `${date}/${month}/${year}`;
console.log(formattedDate);
// output: 7/8/2021
```

As you can see, we used string interpolation to format the date and concatenated the day, month, and year in the desired order.

## Deep Dive

The Date object in Javascript has many other useful methods for accessing and manipulating dates. Some of these methods include `getDay()` (to get the day of the week as a number), `getTime()` (to get the time in milliseconds since January 1, 1970), and `setDate()` (to set a specific date). You can check out the full list of methods on the [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date) for more information.

It's also important to note that the Date object in Javascript is based on the client's system time, so it may differ from the actual current date and time if the system time is incorrect. This can be a potential issue when working with time-sensitive data, so it's always a good idea to check and handle any discrepancies.

## See Also

- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [w3schools: JavaScript Date Object](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Javascript.info: Date and time](https://javascript.info/date)

Now that you know how to get the current date in Javascript, you can confidently add this functionality to your projects. Keep exploring and experimenting with the Date object to see all the possibilities for working with dates in Javascript. Happy coding!