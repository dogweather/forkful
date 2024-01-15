---
title:                "Calculating a date in the future or past"
html_title:           "Rust recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past is a common task in many applications, whether it's for scheduling events, tracking deadlines, or simply for keeping track of time. In Rust, there are efficient and reliable methods for handling dates and times, making it a great language to use for these types of calculations.

## How To
To start off, we need to import the "chrono" crate which provides types and functions for working with dates and times in Rust. We can do this by adding the following line to our "Cargo.toml" file under the "[dependencies]" section:

```Rust
chrono = "0.4.19"
```

Next, we can use the "Utc" type from the "chrono" crate to get the current UTC date and time. This is useful when we want to calculate a date and time in the future or past based on the current UTC time.

```Rust
use chrono::{Utc, Duration};

// Get current UTC time and date
let current_date = Utc::now();
```

### Calculating a future date
Say we want to calculate a future date that is 10 days from now. We can do this by using the "checked_add_signed()" function from the "Duration" type in the "chrono" crate. This function takes in a duration and returns a new date and time based on the current one.

```Rust
// Calculate future date (+ 10 days)
let future_date = current_date.checked_add_signed(Duration::days(10));
```

### Calculating a past date
Similarly, we can calculate a date in the past by using the "checked_sub_signed()" function from the "Duration" type. This function subtracts the specified duration from the current date and time.

```Rust
// Calculate past date (- 30 days)
let past_date = current_date.checked_sub_signed(Duration::days(30));
```

### Formatting the date
The "chrono" crate also comes with a built-in date formatter called "format". This allows us to easily format our dates and times according to a specific format. For example, if we want to display the date in the format "DD/MM/YYYY", we can do the following:

```Rust
// Format the future date
let formatted_date = future_date.format("%d/%m/%Y").to_string();
println!("Future date is: {}", formatted_date);

// Output: Future date is: 01/10/2021
```

## Deep Dive
Behind the scenes, "chrono" uses the "SystemTime" type to get the current date and time. This type is platform-dependent and allows for accurate calculations even if the system clock is changed.

Additionally, the "Duration" type uses the "time::Duration" type from the standard library, making it efficient and reliable for date calculations.

## See Also
- Official "chrono" crate documentation: https://docs.rs/chrono/0.4.19/chrono/
- Rust Standard Library "time::Duration" documentation: https://doc.rust-lang.org/std/time/struct.Duration.html
- Official Rust Language Website: https://www.rust-lang.org/