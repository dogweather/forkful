---
title:                "Переведення рядка в верхній регістр"
html_title:           "Gleam: Переведення рядка в верхній регістр"
simple_title:         "Переведення рядка в верхній регістр"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Капіталізація рядка - це процес зміни першої літери рядка на велику. Програмісти використовують це, наприклад, для форматування виведення даних або поліпшення читабельності коду.

## Як це робити:

Все настільки просто, що вам не запаморочується голова. Подивимось:

```gleam
import gleam/string.{concat, slice, to_upper, to_lower}

fn capitalize(s: String) -> String {
  let first_char = s
  |> slice(0, 1)
  |> to_upper()

  let rest = s
  |> slice(1, None)
  |> to_lower()

  concat([first_char, rest])
}

fn main(_) {
  let _ = capitalize("hello, world")
  |> should.equal("Hello, world")
}
```
Програма вище спочатку вирізає першу літеру з рядка `s`, перетворює її в верхній регістр, потім вирізає решту рядка, перетворює її на нижній регістр, і потім конкатенує (об'єднує) ці дві частини разом.

## Детальніше:

Історично, капіталізація рядків була важливою в типографії для визначення імен, власних назв, та іншого. В програмуванні, це може допомогти уникнути помилок і зрозуміти код краще.

Крім `Gleam`, мови програмування, такі як `Python` і `JavaScript`, також мають вбудовані функції для капіталізації рядків.

Насправді, `Gleam` здійснює капіталізацію рядків через об'єднання трьох операцій: витягування частини рядка (`slice`), конвертації рядка до верхнього/нижнього регістра (`to_upper`/`to_lower`), та конкатенації рядків (`concat`).

## Додатково:

Ось деякі посилання на інші ресурси, які пов'язані з темою:

- Документація `Gleam`: https://gleam.run/docs/
- GitHub `Gleam`: https://github.com/gleam-lang/gleam
- Як капіталізувати рядки в `Python`: https://docs.python.org/3/library/stdtypes.html#str.capitalize
- Як капіталізувати рядки в `JavaScript`: https://developer.mozilla.org/uk/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase