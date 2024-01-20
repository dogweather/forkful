---
title:                "Використання регулярних виразів"
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що та Чому?
Регулярні вирази - це шаблони для пошуку й маніпуляції текстом. Програмісти використовують їх для валідації даних, парсингу та заміни тексту ефективно.

## Як це зробити:
У Gleam застосування регулярних виразів вимагає зовнішньої бібліотеки, такої як `gleam_regex`. Ось базовий приклад:

```gleam
import gleam/regex

pub fn demo() {
  let pattern = regex.from_string("[a-zA-Z]+").unwrap()
  let result = regex.find(pattern, "Hello Gleam")
  case result {
    Ok(matches) -> matches |> regex.Match.group(0) |> io.println
    Error(_) -> "No match found" |> io.println
  }
}
```

При запуску коду виведеться:
```
Hello
```

## Поглиблено:
Регулярні вирази сягають корінням у теорію автоматів 1950-х років. Альтернативами є парсер-комбінатори або написання власних парсерів, але це зазвичай більш складно. У Gleam регулярні вирази реалізовані через бібліотеки, що забезпечують інтерфейс до відповідних можливостей базової мови, такої як Erlang або Elixir.

## Див. також:
- Офіційний сайт Gleam: [https://gleam.run](https://gleam.run)
- Документація `gleam_regex`: [https://hex.pm/packages/gleam_regex](https://hex.pm/packages/gleam_regex)
- Вступ до Regular Expressions: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)