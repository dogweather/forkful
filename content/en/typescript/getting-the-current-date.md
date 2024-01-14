---
title:                "TypeScript recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

In programming, it is often necessary to get the current date and time in order to perform certain tasks. This could range from displaying the current date on a website to calculating time differences between two dates. Knowing how to get the current date can be a valuable skill for any developer.

## How To

To get the current date in TypeScript, we will be using the built-in Date object. This object represents a specific point in time and allows us to access the current date and time information. Let's take a look at how we can use this object in our code:

```TypeScript
// Create a new Date object
const currentDate = new Date();

// Get current date
const date = currentDate.getDate();

// Get current month (months start from 0, so we add 1)
const month = currentDate.getMonth() + 1;

// Get current year
const year = currentDate.getFullYear();

// Get current hours, minutes, and seconds
const hours = currentDate.getHours();
const minutes = currentDate.getMinutes();
const seconds = currentDate.getSeconds();

// Output current date and time
console.log(`${month}/${date}/${year} ${hours}:${minutes}:${seconds}`);
```

Running this code will give us the output of the current date and time in the format of MM/DD/YYYY HH:MM:SS. You can also format the date and time according to your preference using various methods from the Date object.

## Deep Dive

The Date object in TypeScript is based on the JavaScript Date object, so it has all the same functionalities. However, TypeScript provides type annotations which can make working with dates easier and less error-prone.

One interesting thing to note is that the Date object automatically adjusts for time zones, which can be useful when working with dates from different regions. Additionally, the Date object allows for easy manipulation of dates, such as adding or subtracting days, months, or years.

It is important to keep in mind that the Date object represents a specific point in time, so if you need to compare dates, it is recommended to use the getTime() method. This method returns the number of milliseconds since January 1, 1970, which can be easily compared with other date objects.

## See Also

- [TypeScript Date Object Documentation](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [JavaScript Date Object Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)