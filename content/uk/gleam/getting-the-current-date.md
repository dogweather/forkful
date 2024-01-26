---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:14:24.117404-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Що таке та Навіщо?)
To get the current date in programming means snagging the precise moment in time right now. We do this to timestamp events, handle schedules, or cook up reminders – real world tasks need real-time data.

## How to: (Як це зробити:)
Gleam's stdlib doesn't come with a date-time function out of the box, but let's rope in an Erlang function to get the job done. Here's a simple example:

```gleam
import gleam/erlang

fn get_current_date() {
  erlang.date() // Returns a tuple like {2023, 4, 9}
}

pub fn main() {
  let current_date = get_current_date()
  current_date
}
```

Sample output might look like this:

```gleam
{2023, 4, 9}
```

## Deep Dive (Поглиблений аналіз)
Now, why are we borrowing Erlang's powers? Well, Gleam is a static-typed language that compiles to Erlang, so it can use Erlang functions directly. Historically, many languages have their own way to handle dates, but they often lean on underlying system calls. Alternatives? You can install external libraries for more fancy features or different formats, though for basic needs, the Erlang function nails it. 

As for implementation, think about time zones and locale formatting if your project goes global. Erlang's `:calendar` module is a starting point, but more robust timezone support would require the `:tzdata` package, for instance.

## See Also (Дивіться також):
- Gleam's documentation on external libraries: https://gleam.run/book/tour/external-functions.html
- Erlang's `:calendar` module: http://erlang.org/doc/man/calendar.html
- Timezone data package `:tzdata` in Erlang: https://hex.pm/packages/tzdata
