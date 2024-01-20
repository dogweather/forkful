---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:36:29.764085-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що і чому?

Parsing a date means converting a string into a `time.Time` object. Programmers do it to interpret and manipulate dates programmatically, which is key for scheduling, data records, and time-based logic.

## How to:
## Як це зробити:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const layoutISO = "2006-01-02"
	dateStr := "2023-03-15"

	parsedDate, err := time.Parse(layoutISO, dateStr)
	if err != nil {
		fmt.Println("Error parsing date:", err)
		return
	}

	fmt.Printf("Parsed Date: %s\n", parsedDate.Format(time.RFC1123))
}
```

Output:
```
Parsed Date: Wed, 15 Mar 2023 00:00:00 UTC
```

## Deep Dive
## Поглиблений огляд

Go's standard time package uses layout strings as a reference. The layout must show by example how to interpret any date string: use "2006" for the year, "01" for January, and "02" for the second day of the month, based on the specific time -- January 2, 15:04:05 MST 2006.

Alternatives? Sure. You could use third-party libraries like `timeparse` or `dateparse`. But why bother? Go's standard library is powerful and usually sufficient.

Internally, parsing a date consists of tokenizing the string and mapping it to the layout. It's crucial to match the layout to your string's format; otherwise, you'll run into errors.

## See Also
## Дивіться також

- Go by Example: Time Formatting/Parsing: https://gobyexample.com/time-formatting-parsing
- Go's time package documentation: https://pkg.go.dev/time
- Go's layout string explained: https://yourbasic.org/golang/format-parse-string-time-date-example