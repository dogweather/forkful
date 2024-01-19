---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string (the process of date serialization) means changing a date object into a text-based representation. It's a common programming practice to allow easier storage, display, sharing, or manipulation of date data.

## How to:

Here's a super simple way to convert a date to a string in PHP.

```PHP
<?php
   $date = new DateTime(); // current date/time
   $date_str = $date->format('Y-m-d'); // format as a string
   echo $date_str;  // output the string
?>
```
This script spits out the current date in the 'YYYY-MM-DD' format, like this: '2023-07-18'.

Fairly straightforward, right?

## Deep Dive

Historically, PHP's built-in date handling functions have been around since its early days (back to PHP 4). But the more OOP-friendly DateTime class, which we used in our example, didn't arrive until PHP 5.2.0. 

As alternatives, we could use strftime or date_format, but the DateTime::format method apparently has better performance and more options. 

Here comes an interesting point: even though we see the date as a string, PHP internally uses the Unix Timestamp (the number of seconds elapsed since Jan 1, 1970) to handle date and time. Each time we do date-related operations, PHP converts this timestamp to a human-readable date or time, and vice versa. Nifty, isn't it?

## See Also

Want to know more? Here are some recommended sources:

- PHP's official DateTime docs: [php.net/manual/en/class.datetime.php](https://www.php.net/manual/en/class.datetime.php)
- Info on different PHP date/time functions: [www.w3schools.com/php/php_ref_date.asp](https://www.w3schools.com/php/php_ref_date.asp)
- Breathless details on Unix timestamp: [en.wikipedia.org/wiki/Unix_time](https://en.wikipedia.org/wiki/Unix_time) 

Remember, time is an illusion (especially in coding), but deadlines are real! Keep coding, mates!