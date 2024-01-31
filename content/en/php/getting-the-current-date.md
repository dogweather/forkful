---
title:                "Getting the current date"
date:                  2024-01-20T15:15:46.353882-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Grabbing the current date in PHP is all about snatching today's date and time right off the server. Coders do this to track users' actions, timestamp posts, or just to say, "Hey, it's Friday!"

## How to:
You summon the current date in PHP with `date()` function. For example:

```PHP
echo "Today's date is: " . date("Y-m-d") . "<br>";
echo "The time is: " . date("h:i:sa");
```

Output might be:
```
Today's date is: 2023-04-01
The time is: 10:15:36am
```

Simple, right? For UTC time, use the `gmdate()` function:

```PHP
echo "UTC time is: " . gmdate("h:i:sa");
```

Bam, you get something like:
```
UTC time is: 02:15:36pm
```

## Deep Dive
Now that we’ve snatched the basics, let's dig a bit into the ground. First, under the hood, PHP uses the server's system clock for its time functions. Thinking way back, PHP's date functions have a long history, coming from Unix's way of handling dates and times.

Before `date()`, you had `strtotime()` and `mktime()`. These old-timers translate pretty much any English textual datetime description into a Unix timestamp. But they're clunkier for a quick current time grab.

Now, for the nerdy bits – PHP stores dates as integers. Specifically, seconds since the Unix Epoch (Jan 1, 1970). This means the universe according to PHP will go kaboom in 2038 (Google "Year 2038 problem" for the doomsday details). Currently, this isn't a problem since PHP 7 and newer use 64-bit integers, giving us till the end of time... well, almost.

Time zones—these can monkey with your times if you're not careful. Set your default time zone using `date_default_timezone_set()` if you want to sync with a specific watch.

## See Also
Here are some useful docks and tuts to sailing in deeper waters:
- [PHP's date function docs](https://www.php.net/manual/en/function.date.php)
- [Timezones in PHP](https://www.php.net/manual/en/timezones.php)
- [PHP DateTime Class](https://www.php.net/manual/en/class.datetime.php), for fancier ways to deal with dates and times
- That looming [Year 2038 problem](https://en.wikipedia.org/wiki/Year_2038_problem)
