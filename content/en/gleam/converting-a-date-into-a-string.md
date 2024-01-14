---
title:                "Gleam recipe: Converting a date into a string"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting dates into strings is a common task in programming, especially when working with date and time data. By converting a date into a string, it becomes easier to display and manipulate the date in different formats and styles.

## How To

To convert a date into a string in Gleam, we can use the `format` function from the `Time` module. We first need to import the `Time` module, and then we can use it to format the date.

```Gleam
import Time

let date = Time.now()
let string = Time.format(date, "MMMM d, yyyy")
```

The first argument of the `format` function is the date we want to convert, and the second argument is the format we want to use. In this example, we are using the format for the month, day, and year. 

The `format` function accepts a wide range of formatting options, including different date and time components, as well as custom formatting using placeholders. You can refer to the [Gleam documentation](https://gleam.run/documentation/?api=time) for a full list of available options.

```Gleam
import Time

let date = Time.now()
let string = Time.format(date, "The date is {\yyyy} {\M} {\d}")
```

This code will output: "The date is 2021 11 28" 

## Deep Dive

Behind the scenes, the `format` function is using the [ICU (International Components for Unicode) library](http://site.icu-project.org/) to handle the formatting of dates. This library supports a wide range of languages, date and time formats, and localized formatting.

If you need more advanced functionality, such as handling time zones or converting between different calendars, you can use the `ZonedDateTime` and `LocalDateTime` types from the `Time` module. These types provide additional functions for manipulating and formatting date and time data.

## See Also

- [Gleam documentation on Time module](https://gleam.run/documentation/?api=time)
- [ICU library documentation](http://site.icu-project.org/)
- [Blog post on working with dates and times in Gleam](https://example.com/working-with-dates-in-gleam)