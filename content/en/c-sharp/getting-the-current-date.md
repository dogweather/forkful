---
title:    "C# recipe: Getting the current date"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to display the current date on your application or website? Or maybe you wanted to keep track of the date and time the user performed an action. Whatever the reason may be, getting the current date is a common task in programming. In this blog post, we will explore how to get the current date in C# and why it is useful.

## How To

To get the current date in C#, we can use the `DateTime.Now` property. This property returns a `DateTime` object that contains both the date and time, accurate up to the millisecond. Let's see an example of how we can use this property in our code:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine("The current date and time is: " + currentDate);
```

The output of this code will be something like this: `The current date and time is: 12/10/2020 3:25:34 PM`. We can also format the output to only display the date or time, depending on our needs. For example, if we only want to display the current date, we can use the `ToShortDateString()` method. Take a look at the code below:

```C#
DateTime currentDate = DateTime.Now;
string formattedDate = currentDate.ToShortDateString();
Console.WriteLine("Today's date is: " + formattedDate);
```

The output of this code will be `Today's date is: 12/10/2020`.

## Deep Dive

Now that we know how to get the current date in C#, let's take a deeper look at the `DateTime` object and its properties. This object represents a specific point in time and has several useful properties such as `Day`, `Month`, `Year`, `Hour`, `Minute` and `Second`. These properties allow us to access individual components of the date and time. For example, if we want to display the current year, we can use the `Year` property like this:

```C#
DateTime currentDate = DateTime.Now;
int currentYear = currentDate.Year;
Console.WriteLine("The current year is: " + currentYear);
```

The output of this code will be `The current year is: 2020`.

We can also perform operations on `DateTime` objects, such as adding or subtracting days, months or years. This can be useful when dealing with date calculations. For example, if we want to calculate the date 7 days from now, we can do it like this:

```C#
DateTime currentDate = DateTime.Now;
DateTime futureDate = currentDate.AddDays(7);
Console.WriteLine("The date 7 days from now will be: " + futureDate);
```

The output of this code will be something like `The date 7 days from now will be: 12/17/2020 3:25:34 PM`.

## See Also

- [DateTime.Now Property in C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-5.0)
- [Working with Dates and Times in C#](https://docs.microsoft.com/en-us/dotnet/csharp/datetime)
- [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)