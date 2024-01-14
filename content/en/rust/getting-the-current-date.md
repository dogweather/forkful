---
title:    "Rust recipe: Getting the current date"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Getting the current date may seem like a simple task, but it is an important feature in many programs. It allows for time-based functionality, such as scheduling tasks or displaying the current date to the user. In Rust, there are multiple ways to get the current date and it's important to understand how to do so efficiently. 

## How To

To get the current date in Rust, we can use the `chrono` crate. First, we need to add the following to our `Cargo.toml` file:

```
[dependencies]
chrono = "0.4.11"
```

Next, we can use the `chrono::Utc` method to get the current date in UTC format. We can then use `format()` to format the date in our desired way. Here's an example:

```
use chrono::Utc;

let current_date = chrono::Utc::today().format("%Y-%m-%d").to_string();
println!("{}", current_date); // Output: 2020-08-13
```

We can also get the current date and time by using `Utc::now()` and formatting it accordingly. Here's another example:

```
use chrono::Utc;

let current_datetime = chrono::Utc::now().format("%Y-%m-%d %H:%M:%S").to_string();
println!("{}", current_datetime); // Output: 2020-08-13 12:30:45
```

## Deep Dive

Internally, the `chrono` crate uses the `SystemTime` type to get the current date and time. This type is platform-dependent and provides sub-second precision. Additionally, `chrono` also handles time zones and daylight saving time, allowing for accurate and reliable date and time calculations.

When getting the current date in Rust, it's important to consider performance as well. One way to improve performance is by caching the date for a short period, rather than calling the `Utc::now()` method multiple times. This can help reduce unnecessary system calls and improve the overall efficiency of the program.

## See Also

Here are some helpful resources for further reading about getting the current date in Rust:

- [Chrono crate documentation](https://docs.rs/chrono/0.4.11/chrono/)
- [SystemTime API reference](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Performance tips for working with time in Rust](https://blog.jan-ahrens.eu/2017/04/10/rust-performance-tip-1.html)