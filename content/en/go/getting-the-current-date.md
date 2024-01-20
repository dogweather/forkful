---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date means fetching real-time data on the present day, month, and year. Programmers do this to track events, monitor system usage, and establish timelines.

## How to:

Here it is, square one. The basic Go code for getting the current date:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	fmt.Println(time.Now().Format("2006-01-02"))
}
```

Running this will print the current date in the format (YYYY-MM-DD). Let's say, for Brexit day:

```Output
2020-01-31
```

## Deep Dive:

Go's time package gave us this utility, first appearing in Go 1, released back in March 2012. Why "2006-01-02"? It's an educational touch; that particular date was chosen because it's 1, 2, 3... in American formatting. 

You could use methods like `Day()`, `Month()`, and `Year()` on the `Time` struct to get separate components of the date. Here's an alternative approach:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println(t.Year(), t.Month(), t.Day())
}
```

This will print date components separately, like:

```Output
2020 January 31
```

Behind the scenes, Date and Time are represented as the number of nanoseconds elapsed since January 1, 1970 UTC. Aware guys may remark, "Hey, that's UNIX epoch!"

## See Also:

1. Go by Example, [Dates](https://gobyexample.com/time): A short tour, guiding you through time formatting/parsing in Go.
2. Go's Official Documentation, [Time package](https://golang.org/pkg/time/): Bringing all facets of date/time under the spotlight.
3. An explanatory blog post, [The Go time package](https://making.pusher.com/go-time-package/): Unveils the secret world of Go's time manipulation.