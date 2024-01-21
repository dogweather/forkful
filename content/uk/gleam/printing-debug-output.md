---
title:                "Виведення налагоджувальної інформації"
date:                  2024-01-20T17:52:24.969495-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Що таке та навіщо?)
Вивід дебаг-інформації дозволяє бачити, що відбувається всередині програми під час її виконання. Програмісти це роблять, щоб легше виявити і виправити помилки.

## How to: (Як це робити:)
```gleam
// Вивести до консолі просте повідомлення
io.debug("Something happened here")

// Вивести змінну з її значенням
let value = "тестове значення"
io.debug(value)

// Зразок виводу:
// > "Something happened here"
// > "тестове значення"
```

## Deep Dive (Поглиблений огляд)
Printing debug output isn't unique to Gleam. It's a time-honored tradition in programming, dating back to when debugging meant literal bugs in the hardware. Gleam's `io.debug` is convenient for simple debugging but it's not ideal for production environments. Alternatives like structured logging, using external libraries such as `gleam_log` are preferred, where one can scale the verbosity levels and direct logs to various destinations, not just the console.

## See Also (Дивіться також)
- Gleam's `io` module documentation: https://hexdocs.pm/gleam_stdlib/gleam/io/
- A guide to error handling in Gleam: https://gleam.run/book/tour/error-handling.html
- The `gleam_log` library on Hex: https://hex.pm/packages/gleam_log