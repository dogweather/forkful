---
title:    "Javascript recipe: Converting a date into a string"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

When working with dates in a Javascript program, there may be times when you need to convert a date into a string. This could be for displaying the date in a specific format, or for manipulating the date as a string. In this blog post, we will discuss how to convert a date into a string in Javascript.

## How To

Converting a date into a string in Javascript is actually quite simple. Here are a few different methods you can try:

### Using the `toString()` method:

```Javascript
let date = new Date();
let dateString = date.toString();
console.log(dateString); // Output: Wed Feb 10 2021 09:00:00 GMT-0500 (Eastern Standard Time)
```

### Using the `toDateString()` method:

```Javascript
let date = new Date();
let dateString = date.toDateString();
console.log(dateString); // Output: Wed Feb 10 2021
```

### Using the `toISOString()` method:

```Javascript
let date = new Date();
let dateString = date.toISOString();
console.log(dateString); // Output: 2021-02-10T14:00:00.000Z
```

As you can see, each method returns a slightly different string format. Depending on your specific needs, you can choose the method that best suits your requirements.

## Deep Dive

To better understand how these methods work, let's take a deeper dive into the `toString()` method. This method returns a string representing the specified date object, in the local time of the current locale. The returned string format will depend on the implementation, the format may be different for each browser and platform. 

If you want to customize the date format, you can use the `toLocaleDateString()` method, which allows you to specify a locale, as well as options for formatting the date string. Let's see an example:

```Javascript
let date = new Date();
let options = {
    weekday: 'long',
    year: 'numeric',
    month: 'long',
    day: 'numeric'
};
let dateString = date.toLocaleDateString('en-US', options);
console.log(dateString); // Output: Wednesday, February 10, 2021
```

As you can see, by specifying the desired options, we were able to get a custom formatted date string.

## See Also

For more information about date and time manipulation in Javascript, check out the following resources:
- [MDN Web Docs on Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools Date Tutorial](https://www.w3schools.com/js/js_dates.asp)
- [Moment.js library for advanced date and time manipulation](https://momentjs.com/)