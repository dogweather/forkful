---
title:                "Rust recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting dates into strings is a common task in many programming languages, and Rust is no exception. Whether you need to display a date on a user interface or save it in a database, converting a date into a string can be a necessary step. In this blog post, we will explore how to convert dates into strings in Rust.

## How To

To convert a date into a string in Rust, we will use the `to_string()` method provided by the `DateTime` struct from the `chrono` crate. Let's first create a `DateTime` object representing the current date and time:

```Rust
use chrono::prelude::*;

let now = Local::now();
```

Next, we can use the `to_string()` method to convert the `DateTime` object into a string:

```Rust
let date_string = now.to_string();
```

Let's print out the value of `date_string` to the console to see the output:

```Rust
println!("Date: {}", date_string);
```

Output:

```
Date: 2021-10-21 17:30:00.000000000 +0000
```

We can also specify a custom format for the date string by using the `format()` method instead of `to_string()`. For example, let's say we want to display the date in the format "October 21, 2021". We can achieve this by using the `%B %e, %Y` format string, where `%B` represents the full month name, `%e` represents the day of the month, and `%Y` represents the full year:

```Rust
let formatted_date = now.format("%B %e, %Y").to_string();
```

Let's print out the value of `formatted_date` to the console:

```Rust
println!("Formatted Date: {}", formatted_date);
```

Output:

```
Formatted Date: October 21, 2021
```

## Deep Dive

Under the hood, the `to_string()` and `format()` methods use the `Display` trait to convert the `DateTime` object into a string. This trait allows us to customize the format of the date string by implementing the `fmt` method. The `format()` method essentially calls `format!("{}", self)` where `self` is the `DateTime` object. This means we can use any valid format string that we would use with the `format!()` macro.

Additionally, the `DateTime` struct also provides methods for accessing individual components of a date, such as the year, month, and day, which can be useful when constructing customized date strings. You can explore the full list of available methods in the `DateTime` documentation.

## See Also

To learn more about working with dates and times in Rust, check out the following resources:

- [The Official Rust Book - Dates and Times](https://doc.rust-lang.org/book/ch16-01-threads.html)
- [The `chrono` crate documentation](https://docs.rs/chrono)
- [Effective Rust Concurrency - Date and Time Handling](https://www.effective-rust.com/2016/02/01/time-handling.html)