---
title:                "Getting the current date"
date:                  2024-01-20T15:16:25.819098-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

In programming, grabbing the current date helps track events or log data. It's practical for stuff like timestamping, schedules, or just knowing when something happens.

## How to:

```Rust
use chrono::{DateTime, Local};

fn main() {
    let now: DateTime<Local> = Local::now();
    println!("{}", now.format("%Y-%m-%d %H:%M:%S"));
}
```

Output:
```
2023-04-05 14:20:35
```

## Deep Dive

Rust, a systems language focusing on safety and performance, isn't furnished with date and time functionalities in its standard library. Instead, the community builds crates—Rust's term for libraries or packages. One standout is `chrono`.

`chrono` offers rich datetime features. Moreover, it handles time zones, which isn't trivial. The crate uses the time zone data from `IANA` (Internet Assigned Numbers Authority) to represent local dates and times correctly.

Alternative crates like `time` exist but might have different interfaces or features. For leaner needs, `time` can be faster and have fewer dependencies.

Getting local time involves system calls interfacing with the OS. The accuracy and precision might vary and are impacted by the system and its configuration.

The implementation details deserve a nod to the design philosophy too. Rust favors explicitness. So, when you grab the current time, you explicitly choose local time vs. UTC, awareness of time zones, and so forth—minimizing surprise and promoting intentionality in the code.

## See Also:

- Rust's `chrono` crate documentation: https://docs.rs/chrono/
- Rust's `time` crate documentation: https://docs.rs/time/
- `IANA` time zone database: https://www.iana.org/time-zones
