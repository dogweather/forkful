---
title:    "Javascript recipe: Getting the current date"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Do you ever find yourself needing to know what the current date is? Maybe you're creating a to-do list app and want to display the date for each task, or maybe you're building a weather forecast app and need to show the current date on the forecast. Whatever the reason may be, getting the current date is a common need in web development. In this blog post, we'll dive into how to get the current date using Javascript.

## How To

Getting the current date in Javascript is actually quite simple. We can use the built-in Date object to retrieve the current date and time. Let's take a look at an example code snippet:

```Javascript
let today = new Date();
console.log(today);
```

When we run this code, the output will be the current date and time in the format "Day Month Date Year Time". For example, if today is November 19, 2021 and the time is 10:30 PM, the output will be `Fri Nov 19 2021 22:30:00 GMT-0500 (Eastern Standard Time)`. 

But what if we want to display the date in a specific format? We can use various methods provided by the Date object to do just that. For example, let's say we want to display the date in the format "MM/DD/YYYY". We can use the `getMonth()`, `getDate()`, and `getFullYear()` methods to retrieve each component of the date and concatenate them together to form our desired format. Let's take a look at the code:

```Javascript
let today = new Date();
let month = today.getMonth() + 1; // months start from 0, so we add 1 to get the actual month
let date = today.getDate();
let year = today.getFullYear();
let formattedDate = month + '/' + date + '/' + year;
console.log(formattedDate);
```

The output for this code will be `11/19/2021`.

## Deep Dive

Now that we know how to get the current date, let's dive into some more details about the Date object and its methods.

The Date object has various methods that allow us to retrieve different components of the date and time, such as `getMonth()`, `getDate()`, `getFullYear()`, `getHours()`, `getMinutes()`, and so on. These methods return the values of their respective components in numerical form. For example, `getMonth()` will return a number from 0 to 11, where 0 represents January and 11 represents December.

There are also methods available to set different components of the date and time, such as `setMonth()`, `setDate()`, `setFullYear()`, `setHours()`, `setMinutes()`, and so on. These methods allow us to modify the current date and time to a specific value.

Additionally, the Date object also has a `toLocaleString()` method which can be used to display the date and time in a localized format based on the user's language and location.

## See Also

To learn more about the Date object and its methods, check out the following resources:

- MDN web docs on Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- W3Schools tutorial on Date: https://www.w3schools.com/jsref/jsref_obj_date.asp
- FreeCodeCamp article on formatting dates in Javascript: https://www.freecodecamp.org/news/javascript-date-format-explained/