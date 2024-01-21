---
title:                "Перетворення дати в рядок"
date:                  2024-01-20T17:36:34.075457-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення дати у рядок дозволяє записати часову точку в зрозумілому та стандартизованому форматі. Програмісти це роблять для легкого збереження або відображення дати в користувацьких інтерфейсах. 

## Як це зробити:

```gleam
import gleam/io
import gleam/erlang/time.{Date, Format, from_iso_string, to_iso_string}

fn main() {
  let maybe_date = from_iso_string("2023-04-12")
  case maybe_date {
    Ok(date) ->
      let date_string = to_iso_string(date)
      io.println(date_string)
    Error(_) ->
      io.println("Невірний формат дати.")
  }
}
```

Припустимий вивід:
```
2023-04-12
```

## Поглиблений розгляд

Колись дати зберігалися в безлічі форматів, що ускладнювало їх обмін та порівняння. ISO 8601 встановив стандарт, і програмісти часто використовують його для єдності. У Gleam, `to_iso_string` і `from_iso_string` легко перетворюють дати між їх вбудованим представленням та рядками у форматі ISO.

Еквіваленти в інших мовах, як-от JavaScript `Date` об'єкти, мають методи `.toISOString()` та `Date.parse()`. У Gleam, все трохи більш експліцитно, з явним поводженням помилок і зорієнтованість на безпеку типів, забезпечує надійне керування датами.

## Дивіться також

- [ISO 8601 Date and time format overview](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Erlang's calendar module](http://erlang.org/doc/man/calendar.html), основа для роботи з часом у Gleam.