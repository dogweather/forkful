---
title:                "Getting the current date"
html_title:           "Go recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the current date and time in your Go program? Maybe you want to keep track of when a certain event occurred or display the current date on a website. Getting the current date is a common task in programming and luckily, Go makes it easy to do.

## How To

To get the current date in Go, we can use the time package. First, we will import the package at the top of our file:

```Go
import "time"
```

Next, we can use the `Now()` function from the time package to get the current date and time. This function returns a `time.Time` object, which contains all the information about the current date and time. 

```Go
currentTime := time.Now()
```

We can then use a variety of methods on the `currentTime` object to retrieve different components of the current date, such as the year, month, day, hour, minute, and second. Here is an example of how we can get the current month and day and print it out to the console:

```Go
month := currentTime.Month()
day := currentTime.Day()

fmt.Printf("The current date is %s %d.", month, day)
// Output: The current date is July 10.
```

We can also format the date and time in a specific way using the `Format()` method. This method takes in a layout string and returns a string with the desired format. Here is an example of how we can get the current date in the format of "YYYY-MM-DD":

```Go
currentDate := currentTime.Format("2006-01-02")
fmt.Println("Today's date is " + currentDate)
// Output: Today's date is 2021-07-10.
```

## Deep Dive

Behind the scenes, Go uses the `time.Now()` function to get the current date and time from the system clock. This means that the current date and time will depend on the local time zone of the computer where the code is being run. 

We can also change the time zone of the `time.Time` object by using the `In()` method. This method takes in a `*Location` object, which can be retrieved from the `time` package. Here's an example of how we can get the current date and time in a different time zone:

```Go
// Get the current date and time in UTC time zone
utcTime := time.Now().UTC()

// Get the current date and time in PST time zone
pstTime := utcTime.In(time.Location("America/Los_Angeles"))

fmt.Println(utcTime)
fmt.Println(pstTime)
// Output: 2021-07-10 08:30:00 +0000 UTC
// 2021-07-10 01:30:00 -0700 PST
```

We can also perform calculations on the `time.Time` object, such as adding or subtracting a certain amount of time. This can be useful for tasks such as scheduling or creating countdown timers. Here's an example of how we can add 1 hour to the current time:

```Go
newTime := currentTime.Add(time.Hour)
fmt.Println("Time in one hour will be " + newTime.Format("15:04:05"))
// Output: Time in one hour will be 20:30:00
```

## See Also

- [Official Go documentation for time package](https://golang.org/pkg/time/)
- [Tutorial on working with dates and times in Go](https://www.calhoun.io/working-with-dates-and-times-in-go/) 
- [Go playground example of getting and formatting current date and time](https://play.golang.org/p/jWMM6tSafEh)