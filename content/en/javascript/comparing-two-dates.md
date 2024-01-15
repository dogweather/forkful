---
title:                "Comparing two dates"
html_title:           "Javascript recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Comparing two dates is a common requirement in web development when dealing with time-sensitive data, such as events, bookings, or subscriptions. By doing this, we can easily determine which date is earlier or later, and perform different actions accordingly.

## How To
In Javascript, we can compare dates using the `Date()` object and its built-in methods. Here's an example of comparing two dates and getting a clear output of which one is earlier.

```Javascript
let date1 = new Date('2021-01-01');
let date2 = new Date('2020-12-25');

if (date1 > date2) {
  console.log('Date 1 is later than date 2');
} else if (date2 > date1) {
  console.log('Date 2 is later than date 1');
} else {
  console.log('Both dates are equal');
}
```

Output:

```
Date 1 is later than date 2
```

We can also use comparison operators like `>`, `<`, `>=`, `<=` to directly compare two date objects. These operators return a boolean value, true if the condition is met, else false. Here's an example:

```Javascript
let date1 = new Date('2021-01-01');
let date2 = new Date('2020-12-25');

if (date1 >= date2) {
  console.log('Date 1 is later than or equal to date 2');
}
```

Output:

```
Date 1 is later than or equal to date 2
```

## Deep Dive
When comparing two dates, it's important to note that we are actually comparing the time values associated with them. If two dates have the same time, but different timezones, they will still be considered equal. This is because the `Date()` object stores the time in milliseconds, which is converted to the local timezone of the device.

Another important factor to consider is that dates are not just limited to a single day. We can also compare specific time values, such as hours, minutes, and seconds, by using the `getHours()`, `getMinutes()`, and `getSeconds()` methods. Here's an example:

```Javascript
let date1 = new Date();
date1.setHours(10); // set time to 10 AM
let date2 = new Date();
date2.setHours(14); // set time to 2 PM

if (date1.getHours() > date2.getHours()) {
  console.log('Date 1 is later than date 2 in terms of hours');
}
```

Output:

```
Date 1 is later than date 2 in terms of hours
```

## See Also
- [MDN Docs on Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [ECMAScript Language Specification - Dates](https://262.ecma-international.org/11.0/#sec-dates-and-times)