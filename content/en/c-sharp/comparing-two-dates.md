---
title:    "C# recipe: Comparing two dates"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may encounter situations where you need to compare two dates. This could be for tasks such as checking if a deadline has passed, or sorting a list of events by date. By understanding how to compare dates in C#, you can efficiently handle these scenarios in your code.

## How To

To compare two dates in C#, we can use the `Compare()` method of the `DateTime` struct. This method takes two `DateTime` objects as parameters and returns an integer value. Here's an example:

```C#
DateTime date1 = new DateTime(2021, 5, 1);
DateTime date2 = new DateTime(2021, 4, 1);

int result = DateTime.Compare(date1, date2);
```

In this code, we create two `DateTime` objects representing May 1, 2021 and April 1, 2021 respectively. Then, we use the `Compare()` method to compare these two dates. The return value will be 1, indicating that `date1` is after `date2`.

We can use this return value to perform different actions in our code. For example, we could check if the return value is greater than 0 to determine if one date is after another. Additionally, we can use other methods of the `DateTime` struct, such as `Equals()` and `CompareTo()`, for more specific comparison needs.

## Deep Dive

Behind the scenes, the `Compare()` method works by comparing the `Ticks` property of the two `DateTime` objects. The `Ticks` property represents the number of 100-nanosecond intervals that have elapsed since January 1, 0001 at 12:00 AM. This allows for a precise comparison of dates, including the time component.

It's also worth noting that the `Compare()` method is culture-sensitive, meaning it takes into account the current culture of the system. This can cause unexpected results if not handled properly. To avoid this, we can use the `DateTime.Compare()` overload that takes a `DateTimeKind` parameter, specifying the type of comparison we want to perform.

## See Also

For more information on comparing dates in C#, you can refer to the official Microsoft documentation on the `DateTime.Compare()` method (https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare), as well as the `DateTime` struct in general (https://docs.microsoft.com/en-us/dotnet/api/system.datetime).