---
title:    "Javascript recipe: Calculating a date in the future or past"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to calculate a date in the future or past? Maybe you're planning an event and want to see what date it will be in 3 months, or you need to figure out when a bill is due based on its due date and a specific number of days. In these situations, knowing how to calculate a date in the future or past can be a useful skill in your programming arsenal.

## How To
Calculating a date in the future or past in Javascript is actually quite simple, thanks to the built-in `Date()` object. Let's take a look at some examples:

```
// Calculate a date 2 weeks from today
let today = new Date(); // Get today's date
let future = new Date().setDate(today.getDate() + 14); // Set the future date to be 14 days from today
console.log(future); // Outputs the number of milliseconds since January 1, 1970 for the future date

// Calculate a date 1 month ago
let today = new Date(); // Get today's date
let past = new Date().setMonth(today.getMonth() - 1); // Set the past date to be 1 month ago
console.log(past); // Outputs the number of milliseconds since January 1, 1970 for the past date
```

In the first example, we first create a new `Date()` object using `new Date()` and assign it to the `today` variable. Then, we use the `setDate()` method to set the date to be 14 days from today's date and assign it to the `future` variable. Finally, we use `console.log()` to output the number of milliseconds since January 1, 1970 for the `future` date.

In the second example, we use a similar approach to calculate a date in the past, but instead, we use the `setMonth()` method to set the date to be 1 month ago. Again, we use `console.log()` to output the number of milliseconds since January 1, 1970 for the `past` date.

There are many other methods and functions you can use to manipulate dates in Javascript. Take some time to experiment and see what works best for your specific needs.

## Deep Dive
Behind the scenes, the `Date()` object in Javascript actually stores dates as the number of milliseconds since January 1, 1970. This is known as the Unix Epoch. When you call the `setDate()` or `setMonth()` methods, you are actually modifying this underlying value.

It's also worth noting that Javascript's `Date()` object follows the Gregorian calendar, used by most countries around the world. However, there are some edge cases to be aware of, such as Daylight Saving Time and leap years. Therefore, it's always a good idea to test your code thoroughly and account for any potential inaccuracies.

## See Also
For more information on working with dates in Javascript, check out these resources:

- [MDN Web Docs - Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Javascript Date Object](https://www.w3schools.com/js/js_dates.asp)
- [Stack Overflow - How can I add days to a date?](https://stackoverflow.com/questions/563406/add-days-to-javascript-date)

Happy coding!