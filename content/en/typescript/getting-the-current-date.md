---
title:    "TypeScript recipe: Getting the current date"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to display the current date on your website or application? Maybe you want to personalize a message by including the current date or display a countdown to a special event. Whatever the reason, knowing how to get the current date in your TypeScript code is an important skill to have.

## How To
To get the current date in TypeScript, we can use the `Date` object which represents the current date and time. We can then use built-in methods to extract the specific information we need, such as the current day, month, or year.

Let's take a look at a simple example to get the current date and log it to the console:

```TypeScript
const currentDate = new Date(); //creates a new Date object
console.log(currentDate); //outputs the current date and time
```

The output will be in the following format:
`Thu Jul 29 2021 16:53:33 GMT+0530 (India Standard Time)`

We can also use methods like `getDate()`, `getMonth()`, and `getFullYear()` to get the current day, month, and year respectively. These methods return the corresponding number, so we will need to use some conditional statements to convert them into more readable strings.

```TypeScript
const currentDate = new Date(); //creates a new Date object
const day = currentDate.getDate(); //gets the current day
const month = currentDate.getMonth(); //gets the current month
const year = currentDate.getFullYear(); //gets the current year

//conditional statements to convert month number into string
let monthString: string;
if(month === 0){
    monthString = "January";
} else if(month === 1){
    monthString = "February";
} else if(month === 2){
    monthString = "March";
} else if(month === 3){
    monthString = "April";
} else if(month === 4){
    monthString = "May";
} else if(month === 5){
    monthString = "June";
} else if(month === 6){
    monthString = "July";
} else if(month === 7){
    monthString = "August";
} else if(month === 8){
    monthString = "September";
} else if(month === 9){
    monthString = "October";
} else if(month === 10){
    monthString = "November";
} else {
    monthString = "December";
}

console.log(`Today's date is ${day} ${monthString}, ${year}`); //outputs "Today's date is 29 July, 2021"
```

## Deep Dive
The `Date` object has various built-in methods that allow us to manipulate and retrieve information regarding the current date and time. Some commonly used methods include `getDay()` to get the day of the week (0 for Sunday, 1 for Monday, and so on), `getHours()` to get the current hour, `getMinutes()` to get the current minute, and `getSeconds()` to get the current second. We can also use the `setDate()`, `setMonth()`, and `setFullYear()` methods to set a specific date for the current `Date` object.

It's important to note that the `Date` object in TypeScript follows the same rules and limitations as the `Date` object in JavaScript. One of these limitations is that the month is represented by a number from 0 to 11 instead of 1 to 12. This can lead to some confusion, so it's important to keep this in mind when working with dates in your code.

## See Also
- [Date Object in TypeScript - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Manipulating Dates and Times - TypeScript Deep Dive](https://basarat.gitbook.io/typescript/type-system/date-time)
- [Creating a Countdown Timer in TypeScript - DEV Community](https://dev.to/sidhantpanda/countdown-timer-in-typescript-19ac)