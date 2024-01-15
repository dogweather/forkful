---
title:                "Getting the current date"
html_title:           "C# recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
One common task in programming is to get the current date. Whether you need to display the current date on a website or use it for data processing, knowing how to retrieve this information is essential for any programmer. In this article, we will explore the various methods to get the current date in C#.

## How To
To get the current date in C#, we can use the built-in `DateTime` class. This class provides us with methods and properties to work with dates and times. Let's take a look at some examples of how we can use this class to get the current date.

```C#
// Get the current date and time
DateTime now = DateTime.Now;

// Get the current date without the time component
DateTime today = DateTime.Today;
```

In the above examples, we use the `Now` and `Today` properties of the `DateTime` class to retrieve the current date. The `Now` property includes the time component, while the `Today` property returns only the date component with the time set to 12:00:00 AM.

We can also customize the format of the date using the `ToString()` method and a format string. Here's an example:

```C#
// Get the current date in a specific format
string formattedDate = now.ToString("MMM dd, yyyy");
Console.WriteLine(formattedDate);
// Output: Jan 15, 2021
```

In the above code, we use the `ToString()` method with a format string to specify the desired format for the date. In this case, we use the format string "MMM dd, yyyy" to get the month name, day, and year in a specific format.

## Deep Dive
Under the hood, the `DateTime` class uses the system clock to retrieve the current date and time. This means that the date returned by the `Now` and `Today` properties will be the same as the system date and time on the machine where the code is running.

It's worth noting that the `DateTime` class also has a `UtcNow` property, which returns the current date and time in Coordinated Universal Time (UTC). This can be useful when working with international applications or when dealing with time zones.

Additionally, the `DateTime` class has various methods and properties to perform operations on dates. For example, you can use the `AddDays()` method to add a certain number of days to the current date, or the `DayOfWeek` property to get the day of the week for a specific date.

## See Also
- [DateTime.Now Property (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0)
- [DateTime.Today Property (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.today?view=net-5.0)
- [DateTime.UtcNow Property (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.utcnow?view=net-5.0)
- [DateTime Struct (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)

By now, you should have a good understanding of how to get the current date in C# using the `DateTime` class. Whether you need to display the date or perform operations on it, the `DateTime` class has you covered. Happy coding!