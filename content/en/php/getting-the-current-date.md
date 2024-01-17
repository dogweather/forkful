---
title:                "Getting the current date"
html_title:           "PHP recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date refers to obtaining the current date and time from the local time zone. This is a common task for programmers when building websites, applications, or systems that require time-sensitive features. By having access to the current date and time, developers can perform various operations such as scheduling tasks, displaying time-accurate information, or tracking events.

## How to:

To get the current date and time in PHP, you can use the built-in date() function. It takes two parameters - a format and a timestamp. Here's a simple example:

```PHP
date_default_timezone_set('America/New_York');
echo date('Y-m-d H:i:s'); // outputs current date and time in YYYY-MM-DD H:i:s format
```

The first line sets the default time zone to America/New_York to ensure the correct date and time are displayed based on the location. The second line uses the date() function to format the output in the desired format.

You can also use the time() function to get the current Unix timestamp, which represents the number of seconds that have elapsed since January 1, 1970, at 00:00:00 UTC. Here's an example of how to get the current date and time using the Unix timestamp:

```PHP
$timestamp = time();
echo date('Y-m-d H:i:s', $timestamp); // outputs current date and time in YYYY-MM-DD H:i:s format
```

## Deep Dive:

The concept of timekeeping has been around for centuries, with various methods and standards used to measure time. In computing, the Unix timestamp was introduced in the 1970s as a way to accurately store and manipulate time within computer systems. It is based on the Coordinated Universal Time (UTC) and counts the number of seconds since the Unix epoch.

Aside from using the built-in date() and time() functions, developers can also use the DateTime class and its methods to get the current date and time. This class provides more flexibility and functionality compared to the date() function.

In terms of alternatives, some programming languages, such as JavaScript, have their own built-in methods for handling dates and times. However, the concept remains the same - to obtain the current date and time, whether it's through a built-in function or a dedicated class.

## See Also:

- [PHP date() function documentation](https://www.php.net/manual/en/function.date.php)
- [UTC and time zones](https://www.timeanddate.com/time/international-atomic-time.html)
- [DateTime class documentation](https://www.php.net/manual/en/class.datetime.php)