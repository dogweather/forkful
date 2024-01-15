---
title:                "Converting a date into a string"
html_title:           "Rust recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a simple task, but it is an essential skill for any programmer. It allows you to manipulate and display dates in a format that is easily readable for users, making your code more user-friendly.

## How To

Converting a date into a string in Rust is a straightforward process. First, we need to import the `chrono` crate, which is a popular third-party library for handling dates and times. In your `Cargo.toml` file, add the following line under `dependencies`:

```Rust
chrono = "0.4.19"
```

Next, we need to use the `chrono` crate in our code by adding the following line at the top of our `main.rs` file:

```Rust
use chrono::{DateTime, Utc};
```

Now, we can create a `DateTime` object and format it into a string using the `format()` method. Let's say we want to get the current date and time, and display it in the format "dd/mm/yyyy, hh:mm:ss". Our code would look like this:

```Rust
fn main() {
    let current_datetime = Utc::now();
    let formatted_datetime = current_datetime.format("%d/%m/%Y, %H:%M:%S");

    println!("{}", formatted_datetime);
}
```

The output of this code would be something like this: "07/06/2021, 15:45:22".

But what if we want to convert a specific date into a string? We can do that by creating a `DateTime` object with the desired date, instead of using `Utc::now()`. Here's an example:

```Rust
fn main() {
    let my_datetime = DateTime::parse_from_str("2021-06-01 14:00:00", "%Y-%m-%d %H:%M:%S").unwrap();
    let formatted_datetime = my_datetime.format("%d/%m/%Y, %H:%M:%S");

    println!("{}", formatted_datetime);
}
```

This code will display "01/06/2021, 14:00:00" as the output.

## Deep Dive

While the `format()` method is the most commonly used way to convert a date into a string, there are other methods and options available in the `chrono` crate. For example, you can use the `to_rfc2822()` method to get a string representation of the date in the RFC 2822 format, or the `to_rfc3339()` method for the RFC 3339 format.

Additionally, you can also specify your own custom format by using the `strftime()` method. This allows for more flexibility in displaying the date in a specific way. For a full list of available formatting options, check out the `chrono` documentation.

## See Also

- [The `chrono` crate documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Converting Time between time zones in Rust] (https://blog.logrocket.com/converting-time-between-time-zones-in-rust/)
- [Working with Dates and Times in Rust] (https://www.youtube.com/watch?v=BHh_pwoJ2zE)