---
title:                "Використання регулярних виразів"
html_title:           "Gleam: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що і Чому?

Користуватися регулярними виразами в програмуванні означає використовувати шаблони для пошуку та заміни тексту в рядках коду. Програмісти використовують цей підхід для більш ефективної та точної обробки текстових даних, зокрема при перевірці формату вводу користувачів або видаленні непотрібних символів зі строки.

## Як?

```Gleam
import regex

let re = regex": *[A-Z][a-z]+ *"

let name = "John Doe"
let has_name = regex.matches(re, name)
let no_name = regex.matches(re, "jane smith")

println(has_name) // output: True
println(no_name) // output: False
```

## Поглиблене дослідження

Регулярні вирази були розроблені у 1956 році та використовуються у багатьох мовах програмування, включаючи Gleam. Є альтернативи, наприклад, бібліотеки для роботи зі звичайними виразами, але вони зазвичай не так ефективні та прості у використанні. У Gleam регулярні вирази реалізовані за допомогою стандартної бібліотеки та мають широкий функціонал для зручної роботи з текстовими даними.

## Дивіться також

- [Regular Expression Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)