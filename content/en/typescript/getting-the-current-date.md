---
title:                "TypeScript recipe: Getting the current date"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

The current date and time are essential pieces of information in any programming language, including TypeScript. It allows developers to keep track of when a particular action or event occurred, which is crucial for creating accurate and effective applications. Plus, displaying the current date and time adds a level of professionalism and usability to any project.

## How To

To get the current date in TypeScript, we can use the built-in `Date` object. Let's take a look at a basic code example:

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

This code creates a new `Date` object and assigns it to the `currentDate` variable. Then, using the `console.log()` function, we can print the current date and time to the console. The output will look something like this:

```
2021-10-03T21:25:47.203Z
```

We can also format the current date in a more readable way using the `toLocaleString()` method:

```TypeScript
let currentDate = new Date();
console.log(currentDate.toLocaleString());
```

This will output the following:

```
10/3/2021, 9:27:49 PM
```

## Deep Dive

The `Date` object in TypeScript has many methods that allow us to manipulate and format the current date and time. Some useful methods include `getFullYear()`, `getMonth()`, `getDate()`, `getHours()`, `getMinutes()`, and `getSeconds()`, which can be used to extract specific parts of the current date.

We can also manipulate the current date by using the `setFullYear()`, `setMonth()`, `setDate()`, `setHours()`, `setMinutes()`, and `setSeconds()` methods. These methods take in parameters to set the corresponding part of the date. For example:

```TypeScript
let currentDate = new Date();
currentDate.setFullYear(2020);
console.log(currentDate.toLocaleString());
```

This will output:

```
10/3/2020, 9:32:14 PM
```

It's important to note that the `Date` object uses the local time of the user's device. To get the current date and time in a specific timezone, we can use the `toLocaleString()` method with the `timeZone` parameter. For example:

```TypeScript
let currentDate = new Date();
console.log(currentDate.toLocaleString('en-US', {timeZone: 'America/New_York'}));
```

This will output the current date and time in the Eastern Timezone.

## See Also

- [TypeScript Docs: Date](https://www.typescriptlang.org/docs/handbook/2/types-built-in.html#date)
- [Mozilla Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: TypeScript Date Methods](https://www.w3schools.com/TS/ts_dates.asp)