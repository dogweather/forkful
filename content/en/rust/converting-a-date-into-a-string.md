---
title:    "Rust recipe: Converting a date into a string"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why

Many programmers often need to convert dates into strings for various reasons, such as storing and displaying data, or parsing and manipulating dates. Rust provides a powerful Date and Time API that makes this conversion process easier and error-free.

## How To

Converting a date into a string in Rust is a straightforward process. We will be using the `chrono` crate, which is the standard library for handling date and time in Rust.

First, we need to add the `chrono` crate to our `Cargo.toml` file:

```Rust
[dependencies]
chrono = "0.4"
```

Next, we need to import the necessary modules in our Rust code:

```Rust
use chrono::{DateTime, Utc};
```

Then, we can create a date variable and convert it into a `DateTime` object:

```Rust
let date = Utc::now();
let datetime: DateTime<Utc> = date.into();
```

Finally, we can use the `format` function to convert the `DateTime` object into a string with the desired format:

```Rust
let str = datetime.format("%Y-%m-%d %H:%M:%S").to_string();
println!("{}", str);
```

The output will look like this:

```
2021-05-19 12:30:00
```

## Deep Dive

The `format` function uses `strftime` formatting to convert the `DateTime` object into a string. This provides a wide range of options to customize the output according to your needs. Some of the commonly used formatting options are:

- `%Y`: Four-digit year
- `%m`: Zero-padded month (e.g., `05`)
- `%d`: Zero-padded day of the month (e.g., `19`)
- `%H`: 24-hour format of an hour (e.g., `12`)
- `%M`: Minute (e.g., `30`)
- `%S`: Second (e.g., `00`)
- `%P`: AM/PM designation (e.g., `AM` or `PM`)
- `%p`: am/pm designation (e.g., `am` or `pm`)
- `%z`: Offset from UTC (e.g., `+0000`)

For a full list of formatting options, you can refer to the `chrono` crate documentation.

## See Also

- [The Rust Book: Date and Time](https://doc.rust-lang.org/book/ch16-03-dates-and-times.html)
- [Chrono crate documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Playground example](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=61adc81872a77e49f041a19054ecac81)