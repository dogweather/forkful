---
title:                "PHP recipe: Converting a date into a string"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Why 

Converting a date into a string is a common task in PHP programming, especially when working with different date formats or displaying dates on a website. Understanding how to convert a date into a string can make your code more efficient and help you avoid errors. 

##How To 

Converting a date into a string in PHP is relatively simple. You can use the `date()` function, which takes two parameters: a format and a timestamp. The format determines how the date will be displayed and the timestamp represents the date and time you want to convert. Here's an example of converting the current date and time into a string: 

```PHP
$date = date('F j, Y, g:i a');
echo $date;
``` 

This would output something like "October 14, 2021, 3:30 pm". You can also specify a different timestamp to convert, such as a specific date in the future or past. 

To specify the format, you can use a combination of letters and symbols. Some common formats include: 

- `d` - Day of the month, 2 digits with leading zeros
- `M` - A short textual representation of a month, three letters
- `Y` - A full numeric representation of a year, 4 digits

For a full list of available formatting options, check out the [PHP date() documentation](https://www.php.net/manual/en/function.date.php). 

##Deep Dive 

PHP uses Unix timestamps to represent dates and times. This is the number of seconds that have elapsed since January 1, 1970. This means that when you call the `date()` function, it is essentially converting this timestamp into a more readable format. 

One important thing to note is that the default timezone in PHP is set to UTC. This means that if you want to display a date in a different timezone, you will need to specify it using the `date_default_timezone_set()` function before calling the `date()` function. 

Additionally, it's important to ensure that the timestamp you're using is in the correct timezone. This can be the cause of unexpected results when converting dates. To convert a date from a different timezone, you can use the `strtotime()` function to create a timestamp based on a given date string with a specified timezone. 

##See Also 

- [PHP date() documentation](https://www.php.net/manual/en/function.date.php)
- [PHP strtotime() documentation](https://www.php.net/manual/en/function.strtotime.php)
- [List of Timezones to use with date_default_timezone_set()](https://www.php.net/manual/en/timezones.php)