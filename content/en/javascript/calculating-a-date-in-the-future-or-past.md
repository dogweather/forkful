---
title:    "Javascript recipe: Calculating a date in the future or past"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is a common task in programming. It allows you to perform actions based on specific dates, such as scheduling events or setting reminders. It also helps in managing and organizing data within a specific time frame.

## How To
To calculate a date in the future or past, first, you need to understand the different components of a date, such as day, month, and year. Then, you can use built-in Javascript methods to manipulate and add or subtract these components to the current date.

Let's see an example of calculating a future date, say 10 days from now, using the Date object in Javascript.

```
// Store current date in a variable
let currentDate = new Date();

// Use setDate() method to add 10 days to the current date
currentDate.setDate(currentDate.getDate() + 10);

// Output the future date 
console.log(currentDate);
// Output:  Sun May 16 2021 17:15:13 GMT+0530 (India Standard Time)
```

In the above code snippet, the `setDate()` method helps in adding 10 days to the current date. Similarly, you can use other built-in methods like `setMonth()` or `setFullYear()` to calculate different future or past dates.

## Deep Dive
Apart from the built-in methods, Javascript also has external libraries like Moment.js, which makes date and time calculation more convenient and accurate. It provides a wide range of methods and options to manipulate dates and times.

For instance, you can use Moment.js to calculate a date in the future or past by using its `add()` method:

```
// Use add() method to add 10 days to the current date
let currentDate = moment().add(10, 'days');

// Output the future date in a specific format
console.log(currentDate.format('DD-MMM-YYYY'));
// Output: 26-May-2021
```

You can also use Moment.js to calculate relative dates, such as "2 weeks from now" or "3 months ago," which can be useful in creating date-based dynamic content.

## See Also
- [MDN Web Docs on Date methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js documentation](https://momentjs.com/docs/)