---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

# An Unverbose Introduction to Date Comparison in C#

## What & Why?
Comparing two dates in programming is the evaluation of two date-time variables to find out which is earlier, later, or if they are the same. Developers do this when they need to sort events, calculate durations, or trigger actions at specific times.

## How To:
Here's a quick code snippet that shows how it's done in C#:

```C#
DateTime date1 = new DateTime(2022, 01, 01);
DateTime date2 = new DateTime(2022, 06, 01);

int result = DateTime.Compare(date1, date2);

if (result < 0)
     Console.WriteLine("date1 is earlier than date2.");
else if (result == 0)
     Console.WriteLine("date1 is the same as date2.");
else
     Console.WriteLine("date1 is later than date2.");
```
Output

```sh
date1 is earlier than date2.
```

The `DateTime.Compare` method returns an integer that tells us whether the first date is earlier, later, or the same as the second date by returning a negative value, positive value, or zero respectively.

## Deep Dive
C# was released in 2002 and the `DateTime` structure has been there from the beginning due to the importance of date and time manipulation in programming The `DateTime.Compare` function was also there from the start, giving developers a straightforward way to compare date and time data.

You can achieve the same results through subtraction but `DateTime.Compare` is more cognitive-friendly. For example: 

```C#
TimeSpan result = date1 - date2;
// then you evaluate if result is less than, equal to, or greater than TimeSpan.Zero
```
This method works fine but it’s not as clear as using `DateTime.Compare`.

Remember, `DateTime` values are represented as ticks (10 million ticks = 1 second) from the C# datetime epoch (00:00:00 UTC on January 1, 0001). Therefore, the comparison operations are essentially integer comparisons under the hood.

## See Also
For more information about `DateTime` check these sources:
- Microsoft's [DateTime Structure documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [DateTime.Compare method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=net-6.0)
- [TimeSpan Structure](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-6.0)

Happy coding!