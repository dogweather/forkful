---
title:                "Go recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task in programming, especially when dealing with time-sensitive data. Whether you need to check if a certain date is before or after another, or calculate the time difference between them, having the ability to compare dates is essential for any programmer. In this blog post, we will explore how to compare dates using the Go programming language.

## How To

To compare dates in Go, we will be using the built-in `time` package. This package provides functions for working with dates, times, and durations. To start, let's create two date objects using the `time.Date()` function and assign them to variables:

```
start := time.Date(2020, time.October, 1, 0, 0, 0, 0, time.UTC)
end := time.Date(2020, time.October, 10, 0, 0, 0, 0, time.UTC)
```

We have created two date objects for October 1st and October 10th of the year 2020. Now, we can use the `Before()` and `After()` methods to compare these dates. These methods return a boolean value indicating if the first date is before or after the second date, respectively.

```
fmt.Println(start.Before(end)) // Output: true
fmt.Println(end.Before(start)) // Output: false
fmt.Println(start.After(end)) // Output: false
fmt.Println(end.After(start)) // Output: true
```

As we can see, the `Before()` and `After()` methods work as expected. But what if we want to check if two dates are equal? For that, we can use the `Equal()` method:

```
fmt.Println(start.Equal(end)) // Output: false
fmt.Println(end.Equal(end)) // Output: true
```

In addition to these methods, the `time` package also offers a `Sub()` method to calculate the duration between two dates. This method returns a `time.Duration` object, which represents the difference between the two dates in terms of hours, minutes, and seconds.

```
fmt.Println(start.Sub(end)) // Output: -216h
```

## Deep Dive

Behind the scenes, the `time` package stores dates as `time.Time` structs, which contain fields for the year, month, day, and so on. When comparing dates, these fields are compared to determine the relationship between the two dates. It's important to note that when creating a date object using the `time.Date()` function, we are providing values in the UTC timezone. This can cause unexpected results if the local timezone is different. To avoid this, we can use the `time.Now()` function, which returns the current local time.

Another thing to keep in mind is that the `time` package allows us to work with dates in much more detail, such as comparing specific hours, minutes, or even nanoseconds. If you want to learn more about these features, I recommend checking out the official documentation for the `time` package.

## See Also

- [Official `time` package documentation](https://pkg.go.dev/time)
- [Date and Time in Go: A Comprehensive Guide](https://www.calhoun.io/date-and-time-in-go/)

Happy coding!