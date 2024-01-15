---
title:                "दो तारीखों का तुलना करना"
html_title:           "Go: दो तारीखों का तुलना करना"
simple_title:         "दो तारीखों का तुलना करना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyun

Kisi bhi project ya application mein, tarikhon ke maamle ko samajhna aur compare karna bahut zaroori hota hai. Isse hume pata chalta hai ki kisi event ya task ki date sahi hai ya nahi, aur bade projects mein iska importance aur bhi badh jata hai. Isiliye, Go ke saath tarikhon ka comparison sikhna bahut zaruri hai.

## Kaise Karein

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Creating two time variables with different dates
    date1 := time.Date(2020, 12, 1, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2020, 12, 5, 0, 0, 0, 0, time.UTC)

    // Using Before function to compare dates
    if date1.Before(date2) {
        fmt.Println("Date 1 is before Date 2")
    }

    // Using After function to compare dates
    if date2.After(date1) {
        fmt.Println("Date 2 is after Date 1")
    }

    // Using Equal function to check if dates are equal
    if date1.Equal(date2) {
        fmt.Println("Date 1 and Date 2 are equal")
    }
}
```

Yahan par hamne `time` package se `Date` function ka use karke do dates banai hai. Fir, `Before`, `After`, aur `Equal` functions ka use karke humne in dates ko compare kiya hai. In functions ka use karke hum kisi bhi tarikh ki position aur equal status ko check kar sakte hain.

## Deep Dive

Go mein tarikhon ka comparison karne ke liye bahut sare inbuilt functions aur methods available hain. Kuch important functions hain:

1. `Before`: Is function ka use karke hum date ko compare kar sakte hain, jaha hume pata chalta hai ki ek date dusre date se pehle hai ya nahi.
2. `After`: Is function ka use karke hum date ko compare kar sakte hain, jaha hume pata chalta hai ki ek date dusre date ke baad hai ya nahi.
3. `Equal`: Is function ka use karke hum date ko compare kar sakte hain, jaha hume pata chalta hai ki dono dates ek dusre se equal hai ya nahi.
4. `BeforeDate`: Is function ka use karke hume pata chalta hai ki ek date mein usse pehle kitne time hai.
5. `AfterDate`: Is function ka use karke hume pata chalta hai ki ek date mein uske baad kitne time hai.

In functions ko use karke hum tarikhon ka intezaar kar sakte hain aur uske mutabik koi task ya event ko perform kar sakte hain.

## See Also

- [Official Go Documentation for Time package](https://golang.org/pkg/time/)
- [Date and Time in Go](https://www.calhoun.io/parsing-dates-and-times-in-go/)
- [Calculating Date Difference in Go](https://stackoverflow.com/questions/36530251/calculating-the-difference-between-two-dates-in-golang)