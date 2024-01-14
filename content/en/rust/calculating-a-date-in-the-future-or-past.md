---
title:    "Rust recipe: Calculating a date in the future or past"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why 

Calculating dates in the future or past can be a useful skill to have in programming. Whether you need to schedule tasks, set reminders, or plan events, being able to accurately calculate dates can make your programs more efficient and user-friendly. In this blog post, we will explore how to use Rust to calculate dates in the future or past.

## How To

To begin, we will first need to import the necessary libraries for date and time calculations in Rust. We can do this by adding the following lines of code at the top of our Rust file:

```Rust
use chrono::{Duration, NaiveDate};
```

Next, we can create a function that will take in a date and a number of days as parameters and return the calculated date. For example, if we wanted to calculate the date 10 days from today, our function would look like this:

```Rust
fn calculate_date(date: NaiveDate, days: i64) -> NaiveDate {
    let future_date = date.checked_add_signed(Duration::days(days));
    future_date.unwrap()
}
```

Here, we are using the `checked_add_signed` method from the `Duration` library to add the specified number of days to our given date. Then, we return the calculated date.

We can then call our function and print the output to see the result:

```Rust
fn main() {
    let today = NaiveDate::from_ymd(2021, 1, 1);
    let future_date = calculate_date(today, 10);

    println!("{}", future_date);
}
```

Running this code will print out the date 10 days from today, which in this case would be January 11th, 2021.

## Deep Dive

Now that we have a basic understanding of how to calculate dates in the future, let's take a deeper look at how the `checked_add_signed` method works. This method takes in a `Duration` parameter, which allows us to specify the amount of time we want to add to our current date. The `checked_add_signed` method then returns an `Option<NaiveDate>`, which means it can either return the calculated date or `None` if the resulting date is invalid (e.g. February 31st).

Additionally, we can also use the `Duration` library to calculate dates in the past by using the `checked_sub_signed` method instead. This method works in the same way as `checked_add_signed`, except it subtracts the specified duration from the given date.

With these methods and the power of Rust's strong type system and error handling, we can ensure that our date calculations are accurate and error-free.

## See Also 

For more information on the Date and Time library in Rust, check out the official documentation: 

- [Chrono library](https://docs.rs/chrono/0.4.6/chrono/) 
- [Duration methods](https://docs.rs/chrono/0.4.6/chrono/duration/struct.Duration.html)
- [NaiveDate methods](https://docs.rs/chrono/0.4.6/chrono/naive/struct.Date.html)