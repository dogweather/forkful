---
title:    "TypeScript recipe: Calculating a date in the future or past"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is a common task in many programming projects. It allows developers to build dynamic and flexible applications that can handle a wide range of scenarios. With TypeScript, this process becomes even easier and more efficient.

## How To 
To calculate a date in the future or past, TypeScript offers a few built-in methods that make the task simple and straightforward. Let's take a look at some examples:

```TypeScript
//Calculating a Date in the Future
let today: Date = new Date();
let futureDate: Date = new Date();

//Set the date to 5 days from now
futureDate.setDate(today.getDate() + 5);

console.log(futureDate); //Output: 2020-05-14T12:00:00.000Z

//Calculating a Date in the Past
let pastDate: Date = new Date();

//Set the date to 2 weeks ago
pastDate.setDate(today.getDate() - 14);

console.log(pastDate); //Output: 2020-04-29T12:00:00.000Z
```

In the above examples, we first create a new Date object representing today's date. Then, using the `setDate()` method, we can add or subtract a desired amount of days from the current date. This will automatically update the date accordingly. 

## Deep Dive
It's worth mentioning that the `setDate()` method actually returns the milliseconds representation of the date that was set. This allows for further manipulation and calculations if needed. Additionally, there are other built-in methods in TypeScript that can be used to achieve the same result, such as `setFullYear()` for setting the year or `setMonth()` for setting the month.

Another important thing to note is that the `setDate()` method is a mutator method, meaning it modifies the original object rather than creating a new one. This can have implications depending on how your code is structured.

## See Also
- [TypeScript Official Documentation](https://www.typescriptlang.org/docs/home.html)
- [MDN Web Docs - Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Date Methods Tutorial](https://www.javascripttutorial.net/javascript-date-methods/)