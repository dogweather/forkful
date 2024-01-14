---
title:                "Bash recipe: Converting a date into a string"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting dates into strings is a common task in programming, especially in Bash. By converting a date into a string, we can manipulate and display the date in a more easily readable format for users.

## How To
Converting a date into a string in Bash is relatively simple. We can use the `date` command and format it using the `+%m-%d-%Y` option. This will give us the date in the format of month-day-year. Let's see an example:

```Bash
current_date=$(date +%m-%d-%Y)
echo "Today's date is $current_date."
```

The output will be: `Today's date is 10-17-2021.`

We can also specify the date we want to convert by using the `-d` option. For instance, if we want to convert the date of October 1st, 2021, we can use the following code:

```Bash
specific_date=$(date -d "Oct 1 2021" +%m-%d-%Y)
echo "The specific date is $specific_date."
```

The output will be: `The specific date is 10-01-2021.`

We can also format the date in different ways by using other options such as `%b` for displaying the abbreviated month name or `%Y` for displaying the four-digit year. It is important to note that the output is dependent on the locale settings.

## Deep Dive
Converting a date into a string can be more complex if we want to handle different time zones or daylight saving time. In order to get the correct date, we need to specify the time zone using the `-u` option. We can also use the `+format` option to include the time zone in the output. For example:

```Bash
date_string=$(date -u +"%b. %d, %Y %H:%M:%S %Z")
echo "The date and time in UTC is: $date_string."
```

The output will be: `The date and time in UTC is: Jan. 24, 2021 10:30:00 UTC.`

We can also handle daylight saving time by using the `%Z` option to display the time zone's abbreviation. This will take into account the time zone's offset based on daylight saving time.

## See Also
- [Bash Date Command](https://www.gnu.org/software/coreutils/date)
- [Bash Date Format Options](https://www.gnu.org/software/coreutils/manual/html_node/Date-conversion-specifiers.html)
- [Bash Date Timezone Reference](https://www.gnu.org/software/coreutils/manual/html_node/Timezone-Specifiers.html)