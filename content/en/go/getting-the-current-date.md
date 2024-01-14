---
title:    "Go recipe: Getting the current date"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to display the current date in your Go program? Maybe you want to log the date and time that a certain action occurs, or simply show the date on a user interface. Whatever the reason, being able to retrieve the current date is a useful skill for any Go programmer.

## How To

To get the current date in Go, we can use the `time` package. First, we need to import the package in our code:

```
import (
	"time"
)
```

Next, we can use the `Now()` function from the `time` package to get the current date and time. We can assign this to a variable and then format it to our liking. For example, if we want to display the date in the format of "Month/Day/Year", we can use the `Format()` function:

```
currentDate := time.Now()
formattedDate := currentDate.Format("01/02/2006")
```

In the code above, we are using a special layout string "01/02/2006" to specify the format we want. This layout represents the month, day, and year in numeric form. You can use a different layout string based on your desired format.

Once we have the formatted date, we can use it in our code however we want. For example, we can print it out on the console:

```
fmt.Println(formattedDate)
```

This will output the current date in the format we specified: "Month/Day/Year". You can try experimenting with different layouts to display the date in various formats.

## Deep Dive

Underneath the surface, Go uses the Unix timestamp to represent the current date and time. This is the number of seconds that have elapsed since January 1, 1970, 00:00:00 UTC. Go also has a `Unix()` function that converts a given time to its corresponding Unix timestamp.

We can also retrieve the current date and time in different time zones by using the `LoadLocation()` and `In()` functions from the `time` package. For example, if we want to get the current date and time in the Pacific time zone, we can do the following:

```
location, err := time.LoadLocation("America/Los_Angeles")
if err != nil {
	panic(err)
}

currentDate := time.Now().In(location)
```

The `LoadLocation()` function takes a location string as its argument, specifying the time zone you want to use. Then, we can use the `In()` function to convert the current date and time to the chosen time zone.

## See Also

- Go Time Package: https://golang.org/pkg/time/
- Go Date and Time Formatting: https://yourbasic.org/golang/format-parse-string-time-date-example/
- Unix Timestamp Explanation: https://www.epochconverter.com/programming/languages/go