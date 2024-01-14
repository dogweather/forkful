---
title:                "Go recipe: Getting the current date"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

As a beginner to Go programming, it is important to learn how to get the current date as it is a fundamental aspect of many applications. Whether you are building a simple calendar or a complex scheduling system, knowing how to get the current date is crucial.

## How To

In Go, there are multiple ways to get the current date. The most basic way is to use the `time` package and the `Now()` function to get the current date and time. Here is an example of how to do so in Go:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    currentDateTime := time.Now()
    fmt.Println("Current date and time:", currentDateTime)
}
```

The output of this code will be in the format of YYYY-MM-DD HH:MM:SS. If you want to format the output in a specific way, you can use the `Format()` function and specify the desired format using a time layout. For example:

```Go
timeLayout := "January 2, 2006"
currentDate := currentDateTime.Format(timeLayout)
fmt.Println("Today's date is:", currentDate)
```

This will output the current date in the format of "Month Day, Year" such as "October 27, 2021". You can also use other predefined layouts such as "02/01/2006" for "Day/Month/Year" format or "15:04" for 24-hour time.

## Deep Dive

If you are interested in a deeper dive into getting the current date in Go, there are a few important concepts that you should be aware of. Firstly, the `Now()` function returns a `Time` data type which represents a specific point in time. This data type has many useful methods for manipulating, comparing, and formatting dates and times. 

Secondly, the `time` package also has a `Date()` function which allows you to input specific year, month, and day values to create a `Time` data type. This can be useful when working with dates that are not the current date. Lastly, the `Parse()` function in the `time` package allows you to convert a string to a `Time` data type. This can be helpful when receiving date and time inputs from users in a specific format.

By understanding these concepts and familiarizing yourself with the `time` package, you will be able to confidently work with dates and times in your Go programs.

## See Also

- Official Go documentation on `time` package: https://golang.org/pkg/time/
- Tutorial on working with dates and times in Go: https://www.calhoun.io/working-with-dates-and-times-in-go/
- More advanced techniques for handling dates and times in Go: https://blog.learngoprogramming.com/go-for-gold-date-time-ph-739ff3802fb0