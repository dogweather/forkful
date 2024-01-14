---
title:    "Rust recipe: Getting the current date"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Why
In today's world, time is of the essence. Knowing the current date and time is crucial for many applications, whether it's for logging events or displaying real-time data. In this blog post, we'll explore how to get the current date in Rust programming language.

## How To
To get the current date in Rust, we'll be using the standard library's `chrono` crate. First, we'll need to add this crate as a dependency in our `Cargo.toml` file. Simply add the following line:

```
[dependencies]
chrono = "0.4.7"
```

Next, we'll import the `Local` function from the `chrono` crate as follows:

```
use chrono::Local;
```

Now, we can use the `Local::now()` function to get the current date and time. Let's print the date in the following code snippet:

```
let current_date = Local::now().date();
println!("Today's date is: {}", current_date);
```

Running this code will give us the output:

```
Today's date is: 2021-09-19
```

We can also format the date in a specific way using the `format()` function provided by `chrono`. Let's say we want to display the date in the format of "Day, Month Date, Year". We can do so by using the following code:

```
let current_date = Local::now().date();
let formatted_date = current_date.format("%A, %B %e, %Y");
println!("{}", formatted_date);
```

The output for this code would be something like:

```
Sunday, September 19, 2021
```

## Deep Dive
Under the hood, the `Local::now()` function uses the system's clock timezone. It then converts the time into UTC and calculates the date based on that. This ensures that the date remains accurate even if the system's clock timezone changes.

The `format()` function uses formatting codes to specify how the date and time should be displayed. These codes are similar to those used in other languages like C, Python, and PHP.

There are various other functions provided by `chrono` crate for working with dates and times, including creating custom time zones, parsing dates, and calculating time differences.

## See Also
- [Official `chrono` crate documentation](https://docs.rs/chrono/)
- [Rust Programming Language](https://www.rust-lang.org/)

And there you have it, getting the current date in Rust is as simple as that! With the `chrono` crate, dealing with dates and times in Rust becomes much easier and hassle-free. Happy coding!