---
title:                "Написання тестів"
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Навіщо?)
Тести - це код, що перевіряє, чи інший код працює правильно. Програмісти пишуть тести, щоб автоматизувати перевірку свого коду і забезпечити безперебійну роботу програм.

## How to: (Як це зробити:)
```gleam
import gleam/expect
import my_module

pub fn add_test() {
  expect.equal(my_module.add(1, 2), 3)
}

pub fn subtract_test() {
  expect.equal(my_module.subtract(5, 3), 2)
}
```

Sample output:
```
1 test completed, 0 failures.
```

## Deep Dive (Занурення Глибше)
Tests in Gleam are written using the `gleam/expect` module. Historically, testing approached in Gleam are influenced by Elixir's ExUnit and Erlang's common test. Alternative testing frameworks in other languages include pytest (Python), RSpec (Ruby), and jest (JavaScript). The key in Gleam testing is that tests are type-checked, providing another layer of quality assurance for the code.

## See Also (Додатково)
- Helpful Gleam community chat on GitHub Discussions or the #gleam-users on IRC (Libera.Chat) for real-time question-and-answer sessions.