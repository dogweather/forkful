---
title:                "Nykyisen päivämäärän hakeminen"
html_title:           "PHP: Nykyisen päivämäärän hakeminen"
simple_title:         "Nykyisen päivämäärän hakeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Miksi yksi haluaisi hakea nykyisen päivämäärän? Päivämäärän haku on hyödyllistä esimerkiksi kun halutaan näyttää nykyinen päivämäärä tai vertailla päivämääriä toisiinsa.

## Näin teet sen
The `date` function in PHP is used to get the current date. This function takes in two parameters: the format in which the date should be displayed and an optional timestamp. If the timestamp is not specified, the current date and time will be used. Here is an example of how to use the `date` function to display the current date in the format "d.m.Y".

```
<?php
$date = date("d.m.Y");
echo $date;
```

Output: 30.12.2020

To display the current date and time, we can use the format "d.m.Y H:i:s". This will output the date in the format day.month.year hour:minute:second.

```
<?php
$date = date("d.m.Y H:i:s");
echo $date;
```

Output: 30.12.2020 12:34:56

If you want to get the current date and time in a specific timezone, you can use the `date_default_timezone_set` function before calling the `date` function. For example, if we want to get the current date and time in Helsinki, Finland, we would use the timezone "Europe/Helsinki". Here is an example of how to do it:

```
<?php
date_default_timezone_set("Europe/Helsinki");
$date = date("d.m.Y H:i:s");
echo $date;
```

Output: 30.12.2020 14:34:56 (depending on your current timezone)

## Syvempää tietoa
PHP has a built-in function `time()` which returns the current Unix timestamp. This timestamp represents the number of seconds that have elapsed since 1st January 1970 00:00:00 UTC. This timestamp can be used as the second parameter in the `date` function to get the current date and time in a specific format. Here is an example of how to display the current date in a custom format using the `time()` function.

```
<?php
$date = date("d.m.Y", time());
echo $date;
```

Output: 30.12.2020

To get the timestamp for a specific date and time, we can use the `strtotime` function. This function takes in a date and time string and converts it into a timestamp. Here is an example of how to get the timestamp for 25th December 2020 at 12:00 PM.

```
<?php
$timestamp = strtotime("25 December 2020 12:00 PM");
echo $timestamp;
```

Output: 1608888000

## Katso myös
- [PHP date function documentation](https://www.php.net/manual/en/function.date.php)
- [PHP time function documentation](https://www.php.net/manual/en/function.time.php)
- [PHP strtotime function documentation](https://www.php.net/manual/en/function.strtotime.php)