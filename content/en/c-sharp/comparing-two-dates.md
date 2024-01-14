---
title:                "C# recipe: Comparing two dates"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often come across scenarios where we need to compare two dates. Whether it's checking if a user's subscription has expired or determining the age of an account, date comparisons are a common task in many programs. In this blog post, we will explore how to compare two dates using C#.

## How To

To compare two dates in C#, we can use the `DateTime` class and its `Compare()` method. Let's take a look at an example:

```
DateTime date1 = new DateTime(2021, 01, 01);
DateTime date2 = new DateTime(2021, 05, 01);

int result = DateTime.Compare(date1, date2);

if (result < 0)
{
    Console.WriteLine("Date1 is earlier than Date2");
}
else if (result > 0)
{
    Console.WriteLine("Date1 is later than Date2");
}
else
{
    Console.WriteLine("Date1 and Date2 are the same");
}

```

In the above code, we create two `DateTime` objects representing different dates and then use the `Compare()` method to compare them. The method returns an integer value based on the comparison. If the first date is earlier than the second one, the returned value is less than 0. If the first date is later than the second one, the returned value is greater than 0. And if both dates are the same, the returned value is 0.

You can also use the `DateTime` class's `Date` property to compare only the date part of a `DateTime` object and ignore the time component. Here's an example:

```
DateTime date1 = new DateTime(2021, 06, 01);
DateTime date2 = new DateTime(2021, 06, 15);
DateTime date3 = new DateTime(2021, 06, 01, 12, 00, 00);
DateTime date4 = new DateTime(2021, 06, 15, 12, 00, 00);

if (date1.Date == date2.Date)
{
    Console.WriteLine("The dates have the same date component");
}

if (date1.Date == date3.Date)
{
    Console.WriteLine("The dates have the same date component, even though their time component differs");
}

if (DateTime.Compare(date3, date4) == 0)
{
    Console.WriteLine("The dates have the same date and time component");
}
```
In this example, `date1` and `date2` have the same date component, even though their time component differs. And `date3` and `date4` have the same date and time component. We can see the difference when we use the `Compare()` method to compare them.

## Deep Dive

Under the hood, when we use the `Compare()` method of the `DateTime` class, it's actually comparing the `Ticks` property of each `DateTime` object. The `Ticks` property represents the date and time as the number of 100-nanosecond intervals that have elapsed since January 1, 0001 at 00:00:00. So essentially, the `Compare()` method is just comparing two numbers.

It's important to note that when we create a `DateTime` object, it defaults to the local time zone of the computer it's running on. So if you have a program that is being used in different time zones, you might need to take that into consideration when comparing dates.

## See Also

- [Date and Time comparisons in C#](https://docs.microsoft.com/en-us/dotnet/standard/datetime/comparing-dates?redirectedfrom=MSDN)
- [DateTime.CompareTo Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compareto?view=net-5.0)