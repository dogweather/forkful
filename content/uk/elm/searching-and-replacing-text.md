---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що і Чому?

Пошук та заміна тексту - це процес, який допомагає програмістам знаходити специфічні фрагменти тексту та міняти їх на щось інше. Такий механізм полегшує модифікацію великого обсягу даних.

## Як це робити:

Подивимося на приклад із використанням Elm:

```Elm
import String

replaceExample : String
replaceExample =
    let
        originalText = "Привіт, Світе!"
    in
    String.replace "Світе" "Україно" originalText

-- Output: "Привіт, Україно!"
```

У цьому прикладі ми замінюємо слово "Світе" на "Україно" в рядку "Привіт, Світе!".

## Поглиблений Розділ:

- Пошук та заміна тексту в Elm використовують функцію `String.replace`, яка була частиною мови від її ранніх версій.
- Альтернативою може слугувати використання функції `String.slice` для розбиття та заміни частин тексту. Проте, це може бути менш ефективним для великих обсягів даних.
- Механізм пошуку та заміни в Elm працює на базі алгоритму Кнута-Морріса-Пратта, який шукає підрядки в тексті.

## Дивіться Також:

- [Офіційна документація Elm по String.replace](https://package.elm-lang.org/packages/elm/core/latest/String#replace)