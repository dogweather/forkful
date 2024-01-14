---
title:    "Gleam recipe: Converting a date into a string"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a simple task, but it can actually be quite useful in many programming scenarios. By converting a date into a string, you can easily display it in a readable format or use it in calculations and comparisons.

## How To

To convert a date into a string in Gleam, we will use the `|> String.from_date()` function. This function takes in a date value and returns a string representation of it. Let's take a look at this in action:

```Gleam
let date = Time.now()
|> String.from_date()
```

This code will first get the current time using the `now()` function from the `Time` module, and then convert it into a string using the `from_date()` function. The output will be a string like this: `2020-10-22T15:34:58.197Z`. 

But what if we want to display the date in a specific format? We can pass in a formatting string as a second argument to the `from_date()` function. For example, let's say we want to display the date in the format of `YYYY-MM-DD`:

```Gleam
let date = Time.now()
|> String.from_date("YYYY-MM-DD")
```

The output will now be `2020-10-22`. We can also customize the format further by including other date and time elements such as hour, minute, and second. For a full list of available format options, check out the [Gleam documentation](https://gleam.run/documentation/modules/time#format_string).

## Deep Dive

In Gleam, dates are represented by the `Time` type. This type includes information about the current time, such as year, month, day, hour, minute, second, and millisecond. When we convert a date into a string, these values are formatted and displayed according to the chosen formatting string.

It's important to note that the `String.from_date()` function will return a string in the UTC timezone. If you want to display the date in a specific timezone, you will need to use the `Time.zone_offset()` function to adjust the time before converting it into a string.

## See Also

- [Gleam documentation on `String` module](https://gleam.run/documentation/modules/string)
- [Gleam documentation on `Time` module](https://gleam.run/documentation/modules/time)
- [Timezone reference from Microsoft](https://docs.microsoft.com/en-us/azure/azure-stack/user/azure-stack-choose-timezone#:~:text=To%20set%20the%20time%20zone,the%20timezone%20for%20the%20time)

Happy coding with Gleam!