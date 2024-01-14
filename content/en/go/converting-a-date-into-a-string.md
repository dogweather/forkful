---
title:                "Go recipe: Converting a date into a string"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to display a date in a specific format in your Go program? Whether it's for a user interface or for data storage, converting a date into a string is a common task in programming. In this blog post, we'll explore how to do just that in the Go language.

## How To
Converting a date into a string in Go is actually quite simple. We'll be using the `Format` function from the `time` package to achieve this. Let's take a look at an example:

```
Go time := time.Now()
fmt.Println(time.Format("Jan 2, 2006"))
```

In the above code, we first create a `time` variable using the `Now` function from the `time` package, which returns the current local time. Then, we use the `Format` function to specify the desired format for our date using a layout string. In this case, we want the date to be displayed as month, day, and year in the format of "Jan 2, 2006". Running this code will print out the current date in that format.

You can also specify different layouts to get different date formats. Here are a few examples:

```
// Display date and time in RFC3339 format
fmt.Println(time.Format(time.RFC3339))

// Display the current year as 2 digits
fmt.Println(time.Format("06"))

// Display the time in 24-hour clock format
fmt.Println(time.Format("15:04"))
```

As you can see, the `Format` function gives us a lot of flexibility in customizing the date and time output. You can also use it to display dates in other languages by using a translator object with the desired language.

## Deep Dive
The `time` package in Go provides a robust set of functions and constants for working with dates and times. Some additional functions that come in handy when converting a date into a string include `Unix`, which returns the local Time corresponding to the given Unix time, and `Parse`, which parses a formatted string and returns the corresponding time value.

One important thing to note is that the layout string used in the `Format` function follows a specific reference time of "Jan 2, 2006 at 3:04 PM" in the MST (GMT-0700) time zone. This reference time is chosen because it is the Unix timestamp 1136239445 represented in UTC. Therefore, the layout string must be compatible with this reference time in order to get the desired output.

## See Also
- [Go time package documentation](https://pkg.go.dev/time)
- [Layout string reference](https://golang.org/pkg/time/#Time.Format)
- [Working with Time in Go](https://www.digitalocean.com/community/tutorials/working-with-time-in-go)

As you can see, converting a date into a string in Go is straightforward but also offers a lot of flexibility. With the `time` package, you can easily display dates in different formats and languages, making it a useful tool for any programmer working with dates and times in their Go programs. Happy coding!