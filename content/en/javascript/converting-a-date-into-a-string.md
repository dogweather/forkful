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

## Why

Converting a date into a string is a common task in web development. In many cases, we need to display dates in a specific format or manipulate them for various purposes. By converting a date into a string, we can easily work with dates and display them in the desired format.

## How To

To convert a date into a string in Javascript, we can use the `toString()` method. This method returns the string representation of a date object. Let's see an example:

```Javascript
let date = new Date();
let stringDate = date.toString();
console.log(stringDate); // output: Wed Nov 25 2020 11:13:45 GMT+0530 (India Standard Time)
```

We can also use the `toLocaleDateString()` method to convert the date into a string with a specific format based on the user's local time zone.

```Javascript
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
console.log(date.toLocaleDateString('en-US', options)); // output: Wednesday, November 25, 2020
```

We can also use third-party libraries like Moment.js to convert dates into strings with various formats and translations.

## Deep Dive

When converting dates into strings, it's essential to understand the concept of time zones. The `toString()` method returns the date and time in the local time zone of the user's browser. However, the `toLocaleDateString()` method considers the time zone passed as an argument and returns the date and time accordingly.

Another important aspect to consider is that the `toString()` method returns the date and time in a standardized format, while the `toLocaleDateString()` method returns the date and time in a format based on the language specified.

Furthermore, when using third-party libraries like Moment.js, it's crucial to understand their syntax and methods to get the desired format and translations. It's always recommended to check the documentation before using any third-party library.

## See Also

- [MDN web docs on the `toString()` method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toString)
- [MDN web docs on the `toLocaleDateString()` method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleDateString)
- [Moment.js documentation](https://momentjs.com/docs/)