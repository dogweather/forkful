---
title:                "Rust recipe: Converting a date into a string"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

When working with dates in programming, it is often necessary to convert them into a string format. This is because strings are more versatile and easier to manipulate than strictly defined date types. In the Rust programming language, there are specific ways to convert a date into a string, which we will explore in this blog post.

## How To

Converting a date into a string in Rust can be achieved using the `format!` macro. This macro allows you to specify the format in which you want the date to be displayed.

```Rust
use std::fmt::Display;
use chrono::{DateTime, Local};

let date: DateTime<Local> = Local::now(); // Current date and time

let date_string = format!("{}", date); // Converting date into string

println!("{}", date_string); // Output: 2021-06-24 09:45:21.421064+02:00
```

In the example above, we are using the `chrono` crate to get the current date and time in the `Local` timezone. Then, using the `format!` macro, we are converting the date into a string by specifying the `{}` placeholder for the `Display` trait. This will use the default string formatting for dates.

You can also specify a custom string format by using the `DateTime::format()` function.

```Rust
use std::fmt::Display;
use chrono::{DateTime, Local};

let date: DateTime<Local> = Local::now(); // Current date and time

let date_string = date.format("%B %d, %Y"); // Custom format: Month day, year

println!("{}", date_string); // Output: June 24, 2021
```

In this example, we are using the `format()` function to specify a custom string format for the date. In this case, we are displaying the month, day, and year in a specific format.

## Deep Dive

In Rust, dates are represented by the `DateTime` type, which is a part of the `chrono` crate. This type contains all the necessary information about the date, including the year, month, day, hour, minute, second, and timezone. This makes it easier to convert the date into a string, as you have all the information available to create a specific format.

Additionally, the `DateTime` type implements the `Display` trait, which allows it to be converted into a string by using the `format!` macro. This trait is responsible for all types that can be displayed by using the `format!` macro, making it a very powerful tool in Rust.

## See Also

For more information on converting dates into strings in Rust, check out these helpful links:

- [Official Rust documentation on `format!` macro](https://doc.rust-lang.org/std/fmt/macro.format.html)
- [Chrono crate documentation](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Display trait in Rust](https://doc.rust-lang.org/std/fmt/trait.Display.html)

Happy coding!