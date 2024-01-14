---
title:    "Rust recipe: Comparing two dates"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why

As a newer programming language, Rust offers a variety of useful features for developers. One such feature is the ability to easily compare two dates. This may seem like a common task, but it can actually be quite challenging in other languages. In this blog post, we will explore why comparing dates in Rust is beneficial and how to do it effectively.

## How To

To begin, let's create two date variables using the Chrono crate:

```Rust
use chrono::{Utc, TimeZone};

let date1 = Utc.ymd(2021, 10, 1);
let date2 = Utc.ymd(2022, 1, 1);
```

Next, we can compare these dates using the `cmp` method:

```Rust
if date1.cmp(&date2) == std::cmp::Ordering::Less {
    println!("Date 1 is before Date 2");
} else if date1.cmp(&date2) == std::cmp::Ordering::Greater {
    println!("Date 1 is after Date 2");
} else {
    println!("Date 1 is the same as Date 2");
}
```

This will output "Date 1 is before Date 2".

We can also check for equal dates using the `==` operator:

```Rust
if date1 == date2 {
    println!("These dates are equal");
}
```

And we can even add or subtract days from a date using the `with_day` method:

```Rust
let date3 = date1.with_day(7);
let date4 = date2.with_day(1);

date3.cmp(&date4); // This will output "Date 3 is before Date 4"
```

By using these methods and operators, we can easily compare dates in Rust.

## Deep Dive

Under the surface, Rust's date comparison relies on the `Ord` and `PartialOrd` traits. These traits are used for types that can be ordered, making them perfect for comparing dates.

Moreover, the Chrono crate provides an additional `Date` struct with more advanced methods for handling dates, such as checking for leap years and converting time zones. This can be useful for more complex date comparisons.

## See Also

- [Chrono crate documentation](https://docs.rs/chrono/)
- [Rust language official website](https://www.rust-lang.org/)
- [Rust subreddit for discussion and updates](https://www.reddit.com/r/rust/)