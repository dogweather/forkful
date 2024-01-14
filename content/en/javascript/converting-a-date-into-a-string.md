---
title:                "Javascript recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a simple task, but it can actually be beneficial in various programming scenarios. Some common reasons for converting dates into strings include displaying them in a user-friendly format, sorting and manipulating data, and passing data between different systems.

## How To

To convert a date into a string in Javascript, we can use the `toString()` method. This method converts a date object into a string, using the computer's local time zone as the default. Here's an example code block:

```Javascript
let today = new Date(); 
let dateString = today.toString(); 
console.log(dateString); 
```

This code will output the following string: `Mon Nov 09 2020 15:29:11 GMT-0500 (Eastern Standard Time)`

We can also specify a different time zone by using the `toLocaleString()` method. This method takes in two parameters: the desired time zone and an options object to format the string output. Here's an example:

```Javascript
let date = new Date(Date.UTC(2020, 10, 25, 18, 30, 0));

console.log(date.toLocaleString("en-US", {timeZone: "UTC"})); // 11/25/2020, 6:30:00 PM
```

## Deep Dive

Now that we have covered the basic methods for converting a date into a string, let's take a deeper look at the options object for `toLocaleString()`. This object allows us to format the output string according to our preferences.

Some common options include `year`, `month`, `day`, `hour`, `minute`, `second`, `weekday`, and `timeZoneName`. We can specify which options we want to include in the output string and even change the order by specifying an array of options. Here's an example:

```Javascript
let date = new Date("2020-10-20T12:00:00");
console.log(date.toLocaleString("en-US", {weekday: "long", year: "numeric", month: "long", day: "numeric", timeZoneName: "short"})); // Tuesday, Oct 20, 2020, GMT-5
```

For more information on the available options, check out the [MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString). Understanding these options can help us customize our output string to suit our needs.

## See Also

- [MDN documentation for `toString()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [MDN documentation for `toLocaleString()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [W3Schools tutorial on working with dates in Javascript](https://www.w3schools.com/js/js_dates.asp)