---
title:                "Rust recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Rust is a modern, high-performance programming language that has been gaining popularity in recent years. One of the reasons for its popularity is its ability to handle low-level operations efficiently, making it ideal for systems programming. One common operation in many programs is getting the current date. In this blog post, we will explore how to get the current date in Rust and why it is an important task for programmers.

## How To
Getting the current date in Rust is a straightforward process thanks to the standard library's built-in `chrono` crate. The `chrono` crate provides the `Utc` type which represents the current time in the UTC timezone.

To get the current date in Rust, we first need to import the `chrono` crate.

```Rust
use chrono::{Utc, Datelike};
```

Next, we can use the `Utc` type to get the current date and time.

```Rust
let now = Utc::now();
```

We can then access different components of the current date, such as the year, month, and day, by using the methods provided by the `Datelike` trait.

```Rust
let year = now.year();
let month = now.month();
let day_of_month = now.day();
let day_of_week = now.weekday();
```

The `DateTime` type returned by the `Utc::now()` method also allows us to perform operations like formatting the date in a specific way or converting it to a different timezone.

```Rust
let formatted_date = now.format("%B %d, %Y").to_string(); // Output: "August 31, 2021"

let local_date = now.with_timezone(&Local); // Converts current date to local timezone
```

Now, let's see the output of our code.

```Rust
println!("Today's date is {}.", formatted_date);
println!("It is a {}.", day_of_week);
println!("The date in your timezone is {}", local_date);
```

Output:

```
Today's date is August 31, 2021.
It is a Tuesday.
The date in your timezone is August 31, 2021, 5:30 PM IST.
```

## Deep Dive
Behind the scenes, the `Utc::now()` method calls a function provided by the operating system to get the current date and time. This means that the date and time returned may differ depending on the system's settings. Additionally, the `chrono` crate also handles leap years and other time-related complexities, making it a reliable and efficient choice for handling dates and times in Rust.

Furthermore, it is important to note that getting the current date and time can be useful for various purposes, such as logging events, scheduling tasks, or displaying the date on a user interface. It is an essential feature for any programming language and Rust makes it easy and efficient to implement.

## See Also
- [Chrono Documentation](https://docs.rs/chrono/latest/chrono/)
- [Rust Standard Library Documentation](https://doc.rust-lang.org/std/)

Getting the current date in Rust is a simple yet necessary task for any programmer. With the `chrono` crate and Rust's powerful features, handling dates and times becomes an easy and efficient task. So why not give it a try in your next Rust project?