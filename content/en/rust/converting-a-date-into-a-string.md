---
title:    "Rust recipe: Converting a date into a string"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting dates into strings is a task that many programmers encounter in their projects. In Rust, this can be achieved with relative ease thanks to its robust standard library and powerful language features. So, why should you bother converting a date into a string? Well, there are a few reasons:

- Dates are commonly used in various applications, from simple to-do lists to complex financial systems. Converting them into strings allows for easier manipulation and display of this important data.
- String representation of dates is essential for data interchange between different systems. It provides a standardized format that can be easily parsed and understood by other programs.
- When working with user inputs or databases, dates are often stored as strings. Converting them into the appropriate data type (in this case, a date) is necessary for proper processing.

Now that we have a better understanding of why converting a date into a string is important, let's dive into how to actually do it in Rust.

## How To

Converting a date into a string in Rust can be achieved using the `format!()` macro, which is used to construct a string using a given format. Let's take a look at an example:

```Rust
use chrono::prelude::*;

fn main() {
    let current_date = Utc::now(); // get current date and time in UTC timezone
    let date_string = format!("{}", current_date.format("%Y-%m-%d")); // convert date into string using specified format
    println!("{}", date_string); // print the string representation of the date
}
```
Output: `2021-09-25`

In this example, we used the `format!()` macro to convert the `current_date` into a string using the specified format: `"%Y-%m-%d"`, which represents the date in year-month-day format. You can use various other format specifiers to customize the string representation of the date according to your needs.

It's important to note that the `format!()` macro returns a `String` type, so make sure to declare a variable of type `String` to store the converted date string.

## Deep Dive

Now let's take a deeper look at the `format!()` macro and the different format specifiers that you can use to customize the string representation of a date in Rust.

- `%Y`: Year with century as a decimal number (e.g. `2021`).
- `%m`: Month as a zero-padded decimal number (e.g. `09` for September).
- `%d`: Day of the month as a zero-padded decimal number (e.g. `25`).
- `%H`: Hour (24-hour clock) as a zero-padded decimal number (e.g. `15` for 3 PM).
- `%M`: Minute as a zero-padded decimal number (e.g. `30`).
- `%S`: Second as a zero-padded decimal number (e.g. `07`).
- `%w`: Weekday as a decimal number (e.g. `6` for Saturday).
- `%a`: Weekday as an abbreviation (e.g. `Sat`).
- `%A`: Weekday as a full name (e.g. `Saturday`).
- `%b`: Month as an abbreviation (e.g. `Sep`).
- `%B`: Month as a full name (e.g. `September`).

You can also use a combination of these specifiers to create a custom date format. For example, using `"%d %B, %Y"` will result in a date string like `25 September, 2021`.

For a complete list of format specifiers and their descriptions, check out [Chrono's documentation](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html).

## See Also

- [Chrono documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Format string syntax](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html#formatting-dates-and-times)

Now that you know how to convert dates into strings in Rust, you can make use of this knowledge in your future projects. Remember to always refer to the documentation for any additional information or format specifiers that you may need. Happy coding!