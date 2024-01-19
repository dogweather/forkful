---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is a common task in programming where one checks whether a date is before or after another. It's useful for countless applications from implementing scheduling systems to log analysis.

## How to:

Let's jump right into it. Direct comparison of Date objects in TypeScript is a breeze:

```TypeScript 
let date1 = new Date(2020, 11, 20);
let date2 = new Date(2020, 11, 21);

if (date1 < date2) {
  console.log("date1 is earlier");
} else if (date1 > date2) {
  console.log("date1 is later");
} else {
  console.log("dates are equal");
}
```
And the output is:

```
date1 is earlier
```
We can also get the same result without the `<` and `>` operators by using `.getTime()` method:

```TypeScript
if (date1.getTime() < date2.getTime()) {
  console.log("date1 is earlier");
} else if (date1.getTime() > date2.getTime()) {
  console.log("date1 is later");
} else {
  console.log("dates are equal");
}
```
It gives the same output. 

## Deep Dive

There isn't much historical context to this feature. It's simple and has been part of JavaScript(and consequently TypeScript) since early days. Alternatives are mostly language-specific implementations and the method varies with each programming language. For TypeScript We can use libraries like moment.js for more complex date time manipulations.

On getting a bit technical, when a Date object is created, a timestamp gets attached to it. This timestamp is the number of milliseconds since the Unix Epoch (Jan 1, 1970). While comparing two dates, TypeScript simply compares these timestamps. 

## See Also

Basic knowledge about the Date object: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date

In-depth TypeScript documentation: https://www.typescriptlang.org/docs/

If you need more powerful date/time manipulation and formatting: https://momentjs.com/