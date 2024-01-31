---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:50:59.791498-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)

Інтерполяція рядків — це вбудовування змінних у текстовий шаблон. Програмісти це використовують для зручного створення динамічних рядків.

## How to (Як це зробити):

Elm немає вбудованої підтримки інтерполяції рядків, як у деяких інших мовах, але можна легко конкатенувати строки та змінні:

```Elm
name = "Олексій"
greeting = "Привіт, " ++ name ++ "!"

main =
  text greeting
```

Виведе `Привіт, Олексій!`.

## Deep Dive (Поглиблений Аналіз):

Elm сфокусований на ясності і простоті, тому не має вбудованої інтерполяції. Замість цього, конкатенація використовується для створення динамічних рядків – просто об'єднуємо частини за допомогою `++`. У минулому, мови, як Perl чи Ruby, розпочали практику інтерполяції, але Elm тримається підходу без магії, підкреслюючи читабельність і простоту.

Як альтернативу, можна використовувати функції форматування як `String.concat` або більш складні, як `String.format`. Це підходить, коли динамічні рядки стають більш комплікованими:

```Elm
greetUser : String -> String
greetUser name =
  String.concat [ "Привіт, ", name, "!" ]

main =
  text (greetUser "Олексій")
```

## See Also (Дивіться Також):

- Elm `String` module documentation: [package.elm-lang.org](https://package.elm-lang.org/packages/elm/core/latest/String)
- An example of `String.format` from Elm community: [discourse.elm-lang.org](https://discourse.elm-lang.org/) (search for "String.format").
