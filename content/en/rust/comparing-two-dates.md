---
title:                "Rust recipe: Comparing two dates"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When working with dates in programming, it's important to be able to compare them in different ways. This allows for tasks such as sorting events by date or checking if a certain time has passed. In this blog post, we will explore how to compare two dates in Rust.

## How To

To begin, we need to import the `chrono` crate, which provides date and time functionality in Rust. Then, we can create two `DateTime` objects, representing the two dates we want to compare.

```
use chrono::{DateTime, Utc};
let date1 = DateTime::parse_from_rfc3339("2021-07-18T12:00:00+00:00").unwrap();
let date2 = DateTime::parse_from_rfc3339("2021-07-19T12:00:00+00:00").unwrap();
```

Now that we have our dates, we can use various methods to compare them. For example, we can check if `date1` is before `date2`:

```
println!("{}", date1 < date2);
// Output: true
```

We can also compare the dates using their individual components, such as the year, month, or day:

```
println!("{}", date1.year() < date2.year());
// Output: true
```

Another useful method is `duration_since()`, which calculates the difference between two dates in terms of seconds, minutes, hours, etc. This can be helpful when checking if a certain amount of time has passed between two events:

```
let duration = date2.signed_duration_since(date1).num_hours();
println!("{} hours have passed between the two dates.", duration);
// Output: 24 hours have passed between the two dates.
```

## Deep Dive

In addition to the methods mentioned above, there are many other ways to compare dates in Rust. For example, `date1.month()` and `date1.day()` can also be used to compare the months and days of the two dates.

It's also important to keep in mind that comparison of dates is affected by time zones. In the examples above, we used dates with an offset of +00:00, which is UTC time. If we were to change the time zone, the results may be different. Therefore, it's recommended to specify the time zone when creating `DateTime` objects.

## See Also

For more information on working with dates and times in Rust, check out the official `chrono` documentation and other related resources:

- [chrono documentation](https://docs.rs/chrono/)
- [Rust playground with examples](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=38b55765786e1b330d41709b1f74cfb9)
- [Video tutorial on `chrono`](https://www.youtube.com/watch?v=L5GqGiKSoLI)