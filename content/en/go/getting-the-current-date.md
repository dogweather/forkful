---
title:                "Go recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

In any programming language, handling dates and times is an essential aspect of developing applications. In Go, getting the current date is a basic operation that is often needed in order to track events, schedule tasks, or display information. Understanding how to get the current date is crucial for any programmer looking to build robust and reliable applications.

## How To

Getting the current date in Go is a fairly straightforward process. We will first import the time package, which provides functions for manipulating and displaying time-related data. Then, we will use the Now() function from the time package to get the current time.

```
import "time"

currentDate := time.Now()
fmt.Println(currentDate)
```

In the above code, we have used the time.Now() function to get the current date and time. The returned value is in the form of a Time data type, which can be further manipulated or formatted according to our needs. We can also use the Format() function to display the current date and time in a specific format.

```
currentTime := time.Now()
formattedDate := currentTime.Format("01-02-2006")
fmt.Println(formattedDate)
// Output: 07-26-2021
```

We can also get more specific information, such as the current year, month, or day, by using the various methods available in the time package.

```
currentYear := currentTime.Year()
currentMonth := currentTime.Month()
currentDay := currentTime.Day()

fmt.Printf("Current year: %d\nCurrent month: %d\nCurrent day: %d\n", currentYear, currentMonth, currentDay)
// Output: Current year: 2021
// Current month: 7
// Current day: 26
```

## Deep Dive

Understanding how the time package works in Go is crucial for getting the current date accurately. The time package provides a wide range of functions and methods for manipulating and displaying time-related data. One important thing to note is that the time returned by using the Now() function is based on the system's local time zone. If we want to get the time in a specific time zone, we can use the LoadLocation() function.

```
timeZone, _ := time.LoadLocation("Europe/Amsterdam")
currentTime := time.Now().In(timeZone)
fmt.Println(currentTime)
// Output: 2021-07-26 14:55:10.116 +0200 CEST
```

We can also use the Parse() function to convert a string to a Time data type, which can then be further manipulated.

```
dateString := "2021-07-26"
formattedDate, err := time.Parse("2006-01-02", dateString)
fmt.Println(formattedDate)
// Output: 2021-07-26 00:00:00 +0000 UTC
```

## See Also

Now that you have learned how to get the current date in Go, here are some additional resources that you may find helpful:

- [The official Go documentation on the time package](https://golang.org/pkg/time/)
- [A tutorial on working with dates and times in Go](https://flaviocopes.com/go-date-time/)
- [A cheat sheet for time formatting in Go](https://programming.guide/go/format-parse-string-time-date-example.html)