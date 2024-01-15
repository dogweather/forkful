---
title:                "Converting a date into a string"
html_title:           "Go recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Converting a date into a string may seem like a simple task, but it is an essential skill for any Go developer. This conversion allows for easier manipulation and storage of dates in a format that is easily readable and understandable for both humans and machines.

## How To
To convert a date into a string in Go, we can use the `Format()` function from the `time` package. Here is an example code snippet:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now() // get current time
	dateString := now.Format("January 02, 2006") // convert to string in desired format
	fmt.Println(dateString) // output: March 23, 2021
}
```

In the above example, we first use the `time.Now()` function to get the current time and store it in the `now` variable. Then, we use the `Format()` function to convert the time to a string in the format specified by the layout string "January 02, 2006". This layout string is a reference to the input date "January 02, 2006 15:04:05" which is also known as the time layout in Go.

Running this code will output the current date in the specified format. You can also format the date to any other desired format by changing the layout string. Here are a few more examples:

```Go
dateString := now.Format("Jan 02, 2006") // output: Mar 23, 2021
dateString := now.Format("01/02/2006") // output: 03/23/2021
dateString := now.Format("Mon, 01-02-2006 15:04:05") // output: Tue, 03-23-2021 20:10:15
```

## Deep Dive
Under the hood, the `Format()` function uses the `Parse()` function to convert the time to a string. This function takes in a layout string and creates a new time object in the specified layout. Then, the `Format()` function simply returns the date and time components of this object in string format.

It is important to note that the layout string must be in the specific format "Monday, January 02, 2006" to correctly convert the time to a string. Any other format will result in an error.

## See Also
- The Go time package documentation: https://golang.org/pkg/time/
- A tutorial on working with dates and times in Go: https://golangbyexample.com/golang-datetime-example/