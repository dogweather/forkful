---
title:                "Javascript recipe: Comparing two dates"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When working with dates in Javascript, there may come a time when you need to compare two dates. This could be for a variety of reasons, such as checking if a specific date has passed, or finding the difference between two dates. Whatever the reason may be, understanding how to compare dates in Javascript is an essential skill for any programmer.

## How To

To compare two dates in Javascript, we can use the built-in `Date` object. This object allows us to create a variable with a specific date and perform various operations on it. Let's take a look at some examples to better understand how this works.

First, we can create two variables representing two different dates, such as today's date and a future date:

```Javascript
var today = new Date();
var futureDate = new Date("2020/12/31");
```

To compare these two dates, we can use conditional statements such as `if` or `switch`. For example, we can check if the future date is before today's date:

```Javascript
if (futureDate < today) {
  console.log("The future is in the past.");
} else {
  console.log("The future is yet to come.");
}
```

In the above code, we use the less than operator (`<`) to compare the two dates. If the future date is indeed before today's date, the first statement will be executed, otherwise, the second statement will be executed.

We can also find the difference between two dates by subtracting them using the `-` operator. For example, if we want to find the number of days between today and the future date, we can do the following:

```Javascript
var daysDifference = (futureDate - today) / (1000 * 3600 * 24);
console.log("There are " + daysDifference + " days between today and the future date.");
```

In this case, we use some basic math to convert the difference in milliseconds into days. We then print out the result, which would be approximately 495 days in this case.

## Deep Dive

When we compare two dates in Javascript, we are essentially comparing two objects. This means that when we use comparison operators like `==` or `!=`, we are not actually comparing the dates themselves, but rather the objects. In most cases, this will not be a problem, but it is essential to keep in mind when dealing with dates in Javascript.

Also, it is worth noting that the `Date` object in Javascript represents the date and time in the local time zone of the user's computer. This can lead to unexpected results if the users are in different time zones. To avoid this issue, we can use libraries such as Moment.js, which offers timezone support.

## See Also

- [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: Date Methods](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Moment.js](https://momentjs.com/)