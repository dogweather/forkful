---
title:                "Getting the current date"
html_title:           "Rust recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to know the current date and time in your Rust program? Perhaps you want to display it to the user, or use it in a calculation. Whatever the reason, knowing how to get the current date can be a useful skill to have in your programming toolkit.

## How To

To get the current date in Rust, we can use the `chrono` crate. This crate provides date and time handling capabilities, making it easy for us to work with dates in our programs.

To start, we need to add the `chrono` crate to our `Cargo.toml` file:

```Rust
[dependencies]
chrono = "0.4.19"
```

Next, we can use the `Utc::now()` method to get the current date and time in UTC format. This method returns a `DateTime` object, which we can then format as a string using the `format()` method and a format string:

```Rust
use chrono::{DateTime, Utc};

fn main() {
    // Get the current date and time in UTC
    let current_date: DateTime<Utc> = Utc::now();

    // Format the date as a string
    let date_string = current_date.format("%Y-%m-%d");

    // Print the current date
    println!("{}", date_string);
}
```

Running this code will output the current date in the format of `YYYY-MM-DD`.

``` 
2021-09-02
```

## Deep Dive

Behind the scenes, the `DateTime` object returned by the `Utc::now()` method is a combination of a `Date` object and a `Time` object. This allows us to access and manipulate different components of the date, such as the month, day, or year.

Additionally, the `format()` method can take in a variety of format strings, allowing us to format the date and time in different ways. For example, we could use the format string `%A, %B %d %Y` to get a more detailed representation of the current date, including the day of the week and the full month name.

## See Also

- `chrono` documentation: https://docs.rs/chrono/0.4.19/chrono/
- Rust Book: https://doc.rust-lang.org/book/title-page.html
- Official Rust website: https://www.rust-lang.org/