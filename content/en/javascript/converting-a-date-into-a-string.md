---
title:                "Javascript recipe: Converting a date into a string"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a common task in Javascript programming. It allows you to display dates in a specific format that is easier for users to read and understand. Additionally, some APIs may require dates to be in string format, making this skill essential for web development.

## How To

To convert a date into a string in Javascript, you can use the `toString()` or `toLocaleString()` methods. Let's take a look at some coding examples to see how this works:

```
// Converting a date to a string using toString() method
const date = new Date(); // current date and time
const dateString = date.toString(); // converting date to string
console.log(dateString); // output: "Sun May 30 2021 13:30:21 GMT-0400 (Eastern Daylight Time)"
```

```
// Converting a date to a string using toLocaleString() method
const date = new Date(); // current date and time
const options = { weekday: 'long', year: 'numeric', month: 'short', day: 'numeric' }; // customization for output
const dateString = date.toLocaleString(undefined, options); // converting date to string with specified options
console.log(dateString); // output: "Sunday, May 30, 2021"
```

In the first example, the `toString()` method returns the date in a long and specific format, including the time zone. In the second example, the `toLocaleString()` method allows you to customize the output by specifying the date formatting options. You can find a full list of options [here](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString#parameters).

## Deep Dive

There are a few important things to keep in mind when converting dates into strings in Javascript. Firstly, the output of the `toString()` and `toLocaleString()` methods may vary depending on the browser, operating system, and user's locale settings. So, it's crucial to test your code on different environments to ensure consistency.

Secondly, converting dates into strings can be tricky when it comes to time zones. The `toLocaleString()` method uses the user's browser settings for time formatting, which may not be the same as the server where the code is running. This can lead to discrepancies in the output if you are not handling time zones correctly. To avoid this, it's recommended to use a library like [Moment.js](https://momentjs.com/) for more robust and accurate date formatting.

## See Also

- [Date.toString() on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [Date.toLocaleString() on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [Moment.js](https://momentjs.com/)