---
title:                "Go recipe: Comparing two dates"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Comparing dates is a common task when working with time-sensitive data in programming. In this blog post, we will explore how to compare two dates in Go and the important considerations to keep in mind.

## How To
To compare two dates in Go, we first need to convert them into a `time.Time` object. We can do this using the `time.Parse()` function, which takes in a layout and a string representing the date. For example, if we have two dates in the format of "Jan 2, 2006", we can use the following code to convert them into `time.Time` objects:

```Go
date1, _ := time.Parse("Jan 2, 2006", "Jan 1, 2021")
date2, _ := time.Parse("Jan 2, 2006", "Jan 5, 2021")
```

Now that we have our two dates in the form of `time.Time` objects, we can use the `Before()` and `After()` methods to compare them. These methods return a boolean value indicating whether the first date occurs before or after the second date. 

```Go
date1.Before(date2) // returns true
date2.After(date1) // returns true
```

We can also use the `Equal()` method to check if the two dates are exactly the same. 

```Go
date1.Equal(date2) // returns false
```

It's important to note that when using the `After()` and `Before()` methods, we are comparing the dates in relation to each other, not their actual values. This means that even if the dates have different timezones, the comparison will still work correctly.

## Deep Dive
When comparing dates, there are a few things to keep in mind. First, the layout used in the `time.Parse()` function must match the format of the date string we are providing. If they don't match, the date will fail to parse and an error will be returned.

Additionally, dates in Go are stored in UTC timezone by default. This means that if you are working with dates in different timezones, you may need to manually convert them to UTC before comparing them. You can use the `time.UTC()` method to do this.

Another important consideration is how to handle leap years and time zones when comparing dates. Go provides the `time.Date()` function which allows you to create a date with a specific time zone and a leap year indicator. This can be helpful when dealing with complex date comparisons.

## See Also
- [Go Time Package Documentation](https://golang.org/pkg/time/)
- [Working with Time in Go](https://blog.golang.org/using-go-times)
- [Golang Time - Comparing Dates](https://www.golangprograms.com/go-program-to-compare-two-dates.html)