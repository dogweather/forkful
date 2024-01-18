---
title:                "Analisando uma data a partir de uma string."
html_title:           "Javascript: Analisando uma data a partir de uma string."
simple_title:         "Analisando uma data a partir de uma string."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# What & Why?

Parsing a date from a string is the process of converting a date that is in string form, such as "September 5th, 2020", into a date object that can be easily manipulated and used by a programming language like Javascript. Programmers do this in order to work with dates in a more efficient and accurate manner, as well as to compare and calculate dates with ease.

# How to:

```Javascript
\\Example 1: Converting a date string into a date object
const dateStr = "September 5th, 2020";
const date = new Date(dateStr);

console.log(date);
//Output: Sat Sep 05 2020 00:00:00 GMT-0400 (Eastern Daylight Time)
``` 

```Javascript 
\\Example 2: Manipulating a date object to get the current year
const currentDate = new Date();
const currentYear = currentDate.getFullYear();

console.log(currentYear);
//Output: 2020 
```

# Deep Dive

Parsing dates from strings has been a common problem for programmers, especially in the early days of computing. Before the introduction of standardized date formats, different regions and cultures would have their own way of writing dates, leading to confusion in the digital world. As a result, the ISO 8601 format was created to provide a universal and unambiguous way of representing dates.

In addition to using the standard Date object in Javascript, there are also alternative libraries such as moment.js and date-fns that offer more advanced functionalities for parsing and manipulating dates.

When parsing dates from strings, it is important to keep in mind the time zone and locale of the date string in order to accurately convert it into a date object.

# See Also

To learn more about the Date object in Javascript, check out the official documentation from Mozilla: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date 

For more advanced date parsing and manipulation, take a look at moment.js and date-fns: https://momentjs.com/ https://date-fns.org/