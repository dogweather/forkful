---
title:                "Converting a date into a string"
html_title:           "Javascript recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string means taking a specific date and time and converting it into a text format that is easily readable by humans. Programmers do this so that they can display the date in a desired format for the user, such as "May 17, 2021" instead of "05/17/2021."

## How to:

```Javascript
// Using the toDateString() method, which returns a date in the format "Month Day, Year"
let date = new Date(); // creates a new Date object with current date and time
console.log(date.toDateString()); // outputs "Mon May 17 2021"

// Using date-fns library, which allows for custom date formatting
const { format } = require('date-fns');
const date = new Date();
console.log(format(date, 'MMMM dd, yyyy')); // outputs "May 17, 2021"
```

## Deep Dive:

Converting a date into a string has been a key feature in programming languages since the early days of computing. Before the introduction of standard libraries or built-in methods, developers had to manually convert dates into strings using various algorithms. However, with the advancement of programming languages, this process has become much simpler and streamlined.

In addition to the methods mentioned above, there are also other ways to convert dates into strings in Javascript. These include the toLocaleDateString() method, which allows for more customized date formatting based on the user's locale, and the use of third-party libraries such as Moment.js and Luxon.

Under the hood, converting dates into strings involves using the built-in Date object and its properties and methods. The toDateString() and toLocaleDateString() methods, for example, utilize the built-in Date.prototype.toString() method to convert the date object into a string. It is important for developers to understand the inner workings of these methods in order to properly utilize them in their code.

## See Also:

- [Date.prototype.toDateString() on MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toDateString)
- [date-fns library documentation](https://date-fns.org/docs/format)