---
title:                "Comparing two dates"
html_title:           "Rust recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates is a common operation in programming that involves checking if one date is before, after, or equal to another date. Programmers often need to compare dates in their applications for various reasons, such as sorting data, finding date range overlaps, or checking for specific events or deadlines.

## How to:
To compare two dates in Rust, you can use the `cmp` method provided by the `PartialOrd` trait. Let's see some examples:
```
use std::cmp::Ordering;

// Compare two dates using the `cmp` method
let date1 = "2021-08-21".parse::<NaiveDate>().unwrap();
let date2 = "2021-09-01".parse::<NaiveDate>().unwrap();
let result = date1.cmp(&date2);

// Check the result
match result {
    Ordering::Less => println!("Date 1 is before Date 2"),
    Ordering::Equal => println!("Date 1 is equal to Date 2"),
    Ordering::Greater => println!("Date 1 is after Date 2"),
}
```
Output:
```
Date 1 is before Date 2
```

You can also use the `<`, `<=`, `==`, `>=`, and `>` operators to compare two dates. These operators call the `cmp` method internally and return a `bool` value representing the result. For example:
```
use std::cmp::Ordering;

// Compare two dates using the `<` operator
let date1 = "2021-08-21".parse::<NaiveDate>().unwrap();
let date2 = "2021-09-01".parse::<NaiveDate>().unwrap();
let result = date1 < date2;

// Check the result
if result {
    println!("Date 1 is before Date 2");
} else {
    println!("Date 1 is equal to or after Date 2");
}
```
Output:
```
Date 1 is before Date 2
```

## Deep Dive:
Comparing dates has been a challenge for programmers since the early days of computing. In the past, dates were often stored as three separate integers representing day, month, and year. This made it difficult to compare dates accurately, especially when accounting for leap years and different calendar systems. Modern programming languages, like Rust, have built-in date/time types and methods to make date comparison easier and more accurate.

In addition to the `cmp` method, Rust also provides the `max` and `min` methods to compare two dates and return the latest or earliest date, respectively. These methods are useful when working with date ranges and need to find the beginning or end date.

If you're looking for alternatives to the built-in date/time types in Rust, you can check out external crates like `chrono` or `time`. These crates offer more advanced date/time functionalities and might better suit your use case.

## See Also:
- [Standard Library: chrono - Date and time handling in Rust](https://doc.rust-lang.org/chrono/index.html)
- [RustDose - Comparing dates in Rust](https://www.rustdose.com/post/rust-comparing-dates/)