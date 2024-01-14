---
title:    "PHP recipe: Converting a date into a string"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Why

Converting dates into strings may seem like a simple task, but it can be a crucial one in many programming projects. By converting dates to strings, developers can easily manipulate and display dates in a more human-readable format. This can be especially helpful in creating user interfaces or generating reports.

## How To

To convert a date into a string in PHP, you will need to use the `date()` function. This function accepts two parameters: a format and a timestamp.

```
<?php
$date = strtotime("2020-08-06"); //timestamp for August 06, 2020
echo date("F j, Y", $date); //outputs August 6, 2020
?>
```

In the above example, we first use the `strtotime()` function to convert a string date into a timestamp. Then, we use the `date()` function to format the date according to our desired format. In this case, we use "F j, Y" to display the date in the format of "Month day, Year".

Other commonly used format characters include "m" for month, "d" for day, and "Y" for the year. You can find a complete list of format characters and their meanings in the [PHP manual](https://www.php.net/manual/en/function.date.php).

## Deep Dive

One common challenge when converting dates into strings is dealing with different time zones. In PHP, you can set the default time zone using the `date_default_timezone_set()` function. This will ensure that all date and time related functions will use the specified time zone.

```
<?php
date_default_timezone_set("America/New_York"); //set default time zone to New York
$date = strtotime("2020-08-06"); //timestamp for August 06, 2020
echo date("F j, Y", $date); //outputs August 6, 2020 in New York time
?>
```

Another useful function for date and time manipulation is the `strtotime()` function. This function can read most English language representations of dates and times and convert them to a timestamp. For example, you can use phrases like "next week" or "last Thursday" to get a timestamp for those dates.

```
<?php
$date = strtotime("next Monday"); //timestamp for next Monday
echo date("F j, Y", $date); //outputs the date for next Monday

$date = strtotime("last Thursday"); //timestamp for last Thursday
echo date("F j, Y", $date); //outputs the date for last Thursday
?>
```

## See Also
- [PHP date function manual](https://www.php.net/manual/en/function.date.php)
- [PHP strtotime function manual](https://www.php.net/manual/en/function.strtotime.php)
- [List of PHP time zones](https://www.php.net/manual/en/timezones.php)

Converting dates into strings may seem like a simple task, but it can offer many benefits in terms of display and manipulation. By using the built-in date and time functions in PHP, developers can easily convert dates into any format they desire. Knowing how to convert dates to strings can greatly enhance the user experience of your application.