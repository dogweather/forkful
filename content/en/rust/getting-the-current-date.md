---
title:                "Rust recipe: Getting the current date"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

When developing a program, it is often necessary to have access to the current date. This can be used for various purposes such as logging, organizing data, or simply displaying the current date for the user. In Rust, there are various ways to get the current date and this blog post will explore some of them.

## How To

There are multiple ways to get the current date in Rust, depending on your specific needs. Let's take a look at three different methods using the built-in `std::time` library.

```Rust
use std::time::SystemTime;

// Method 1: Using `SystemTime::now()`
let current_date = SystemTime::now();
println!("{:?}", current_date); // Example output: Ok(2021-10-06 18:37:17.123456 +0000)

// Method 2: Using `SystemTime::now().duration_since()`
let current_date = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_secs();
println!("{}", current_date); // Example output: 1633541830 (number of seconds since UNIX epoch)

// Method 3: Using `SystemTime::now().format()`
use chrono::Local;
let current_date = Local::now().format("%Y-%m-%d %H:%M:%S").to_string();
println!("{}", current_date); // Example output: 2021-10-06 18:37:17
```
In the first method, we simply use the `now()` method from `SystemTime` to get the current date and time. This returns a `Result` type, which we can then print using the `println!` macro. Note that this method includes the current timestamp in nanoseconds.

The second method uses the `now()` method again, but this time we call the `duration_since()` method and pass in `SystemTime::UNIX_EPOCH` as an argument. This returns the duration between the current date and the UNIX epoch, which is January 1, 1970. We then use the `as_secs()` method to convert this duration into the number of seconds and print it out.

Finally, the third method uses the `format()` method from the `Local` struct in the `chrono` library. This method allows us to format the date and time in any way we want by using a format string. In this case, we specify the year, month, day, hour, minute, and second in a specific order. This returns a `String` type, which we can then print using the `println!` macro.

## Deep Dive

Now, let's take a deeper look at how the `now()` method works under the hood. When we call this method, it retrieves the current time from the system and returns a `SystemTime` instance. This instance contains a `Duration` type, which represents the amount of time since the UNIX epoch. This duration is then used to calculate the current date and time.

It's worth noting that the `now()` and `duration_since()` methods use the `get_time()` function from the `time` crate in order to get the current time. This function makes a system call to retrieve the current time, which can be expensive. That's why it's recommended to store the current time in a variable and reuse it instead of constantly calling the `now()` method.

## See Also

- [std::time::SystemTime](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [chrono::Local](https://docs.rs/chrono/0.4.19/chrono/offset/local/index.html)
- [time::get_time()](https://docs.rs/time/0.2.25/time/fn.get_time.html)