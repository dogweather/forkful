---
title:    "Rust recipe: Calculating a date in the future or past"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why 

Calculating dates in the future or past may seem like a simple task, but it can quickly become complicated when dealing with different time zones, daylight saving time, and different calendar systems. Rust offers a robust and efficient way to handle such date calculations, making it a popular choice for many programmers.

## How To 

To calculate a date in the future or past using Rust, we can first start by importing the `chrono` crate which provides utilities for date and time manipulation. 

```rust
use chrono::{Utc, NaiveDate, Duration};
```

Next, we need to specify a starting date, which can be achieved using the `NaiveDate` class provided by `chrono`. For example, if we want to calculate 30 days from today, we can use the `today()` method to get the current date and then add 30 days to it.

```rust
let start_date = Utc::today().naive_local();
let future_date = start_date + Duration::days(30);
```

To calculate a date in the past, we can simply subtract the desired number of days instead.

```rust
let past_date = start_date - Duration::days(30);
```

We can also specify a specific date using the `NaiveDate::from_ymd()` method, which takes year, month, and day as parameters. 

```rust
let date = NaiveDate::from_ymd(2021, 9, 1);
```

We can even calculate dates in months and years instead of days using the `Duration` class. For example, to calculate a date 3 months in the future, we can use `Duration::months(3)`.

```rust
let future_date = start_date + Duration::months(3);
```

Once we have our desired dates, we can print them out using the `format()` method and specify the desired format.

```rust
println!("Future date: {}", future_date.format("%Y-%m-%d"));
println!("Past date: {}", past_date.format("%Y-%m-%d"));
```

The output would be:

```
Future date: 2021-10-16 
Past date: 2021-08-16 
```

## Deep Dive 

When dealing with date calculations, it's important to consider different time zones and daylight saving time. The `Utc` class provided by `chrono` ensures that all date and time values are converted to UTC for consistency. 

Additionally, `chrono` also supports different calendar systems, such as the Gregorian, Julian, and Islamic calendars. This allows for accurate calculations regardless of which calendar is used.

It's also worth noting that the `NaiveDate` class provides methods for converting dates to local timezone if needed.

## See Also 
- [Chrono Documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Language](https://www.rust-lang.org/)
- [Understanding Date and Time in Programming](https://www.freecodecamp.org/news/understanding-date-and-time-in-programming/)

Overall, Rust offers a powerful and easy-to-use solution for calculating dates in the future or past. With the `chrono` crate, we can handle different time zones and calendar systems seamlessly, making it a reliable choice for any date-related tasks. Hopefully, this article has given you a good understanding of how to perform date calculations using Rust. Happy coding!