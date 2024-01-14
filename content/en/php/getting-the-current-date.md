---
title:                "PHP recipe: Getting the current date"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why 

Have you ever needed to get today's date in your PHP code? Maybe you want to display the current date on your website, or use it to calculate the age of a user. Whatever the reason may be, knowing how to get the current date in PHP can be a useful skill to have in your programming toolkit. In this blog post, we'll explore how to get the current date in PHP and dive deeper into the different ways you can do it. 

## How To 

Getting the current date in PHP is a fairly straightforward process. There are several different ways to do it, depending on your specific needs. Let's take a look at some examples: 

```
<?php 
// Example 1 - using the date() function 
$date = date('Y-m-d'); 
echo $date; // Output: 2021-05-19 

// Example 2 - using the DateTime class 
$dateObj = new DateTime(); 
$date = $dateObj->format('Y-m-d'); 
echo $date; // Output: 2021-05-19 

// Example 3 - using the Carbon library 
$date = Carbon::now()->format('Y-m-d'); 
echo $date; // Output: 2021-05-19 
?> 
```

In the first example, we use the built-in `date()` function. This function accepts a format parameter and returns the current date in the specified format. In this case, we use `'Y-m-d'` to get the date in the year-month-day format. 

In the second example, we use the `DateTime` class, which provides more functionality for working with dates and times in PHP. We create a new `DateTime` object and call the `format()` method to get the current date in our desired format. 

Finally, in the third example, we use the popular Carbon library, which makes working with dates and times in PHP even easier. We use the `now()` method to get the current date and call the `format()` method to format it. 

## Deep Dive 

Now that we've explored some examples of how to get the current date in PHP, let's dive deeper into each of these methods and understand how they work. 

### The `date()` function 

The `date()` function is a built-in PHP function that returns the current date and time in the specified format. It accepts two parameters - format and timestamp. The format parameter is required, and it tells the function how to format the date. The timestamp parameter is optional and is used if you want to get the date from a specific timestamp instead of the current date. 

### The `DateTime` class 

The `DateTime` class was introduced in PHP version 5.2 and is the recommended way of working with dates and times in PHP. It provides a more object-oriented approach and has many useful methods for manipulating dates. 

### The Carbon library 

The Carbon library is a popular PHP library for working with dates and times. It provides a fluent interface and makes working with dates much easier. It also has many convenient methods for working with dates, such as adding and subtracting days, weeks, and months. 

## See Also 

- PHP Documentation: [Date and Time functions](https://www.php.net/manual/en/datetime.format.php)
- Laravel Documentation: [Carbon](https://laravel.com/docs/8.x/eloquent)