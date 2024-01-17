---
title:                "Calculating a date in the future or past"
html_title:           "Javascript recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculating a date in the future or past involves using a programming language like Javascript to determine a specific date by adding or subtracting days, months, or years from a given date. Programmers use this to automate tasks such as scheduling appointments, sending reminders, or setting deadlines.

## How to:
To calculate a date in the future or past in Javascript, use the `getDate()`, `getMonth()`, and `getFullYear()` methods to get the current date values. Then, use the `setDate()`, `setMonth()`, and `setFullYear()` methods to manipulate the date. Finally, use the `toDateString()` method to convert the date object into a readable format.

Example: 
```
//Calculating 10 days in the future
var today = new Date(); //returns current date
today.setDate(today.getDate() + 10); //sets new date 10 days in the future
console.log(today.toDateString()); //displays new date as a string
```
Output: 
```
Sat Jan 16 2021
```
## Deep Dive:
- Historical context: Before the emergence of programming languages, calculations involving dates were done manually. However, this was prone to human error and time-consuming. With the advancement of technology, programmers were able to automate this process, making it more accurate and efficient.

- Alternatives: While Javascript is a popular choice for calculating dates, there are other programming languages such as Python, Java, and C++ that also have date manipulation capabilities.

- Implementation details: Calculating dates in the future or past involves understanding the syntax of the programming language being used and the methods specific to dates. It also requires knowledge of date formatting to ensure the desired output is achieved.

## See Also:
To learn more about calculating dates in the future or past in Javascript, check out these resources:
- [MDN web docs on Dates in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools tutorial on Javascript Dates](https://www.w3schools.com/js/js_dates.asp)
- [Date-fns library for Javascript](https://date-fns.org/)