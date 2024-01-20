---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# An Easy Date with Rust: The Present, Its Core 

## What & Why?

Retrieving the current date in Rust allows you to know the exact time your program is running. It's often used in logging, tracking time-based events, or generating date-stamped files.

## How To:

Here’s a no-nonsense example using Rust's `chrono` library:

```Rust
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("{}", now);
}
```
Execute this code, and your output will mirror the exact moment when you ran your program. It may appear like the following:

```Output
2024-06-23 06:21:41.104690 UTC
```
Pleased? That's your program gleaming the present in terms of year, month, day, hour, minute, and second!

## Deep Dive

Here’s the historical and further detail. 

The `chrono` crate was born around Rust's 1.0 release. Before that, tier-one date and time management just didn't exist within Rust.

There's an alternative to `chrono` called `time`, simpler and more minimalistic. However, `chrono` has established itself better due to the rich functionality.

Implementation-wise, fetching the current date entails querying the system for the current time. The system gets this information from your computer's internal clock. 

## See Also:

For a thorough education on date and time management within Rust, check out the documentation:

- [`chrono` documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [`time` documentation](https://docs.rs/time/0.2.27/time/) 

Additionally, to stay updated with Rust’s development as a language, keep up with [Rust's blog](https://blog.rust-lang.org/). 

Congratulations! You’ve just taken a step deeper into the world of Rust programming.