---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо це потрібно?
Інтерполяція рядка - це процес вставки значень змінних прямо в рядок. Програмісти використовують цей прийом, щоб легше і ясніше форматувати рядки.

## Як це зробити:
Elm не має вбудованої строкової інтерполяції, як інші мови. Ми будемо використовувати функцію `++` для об'єднання рядків. Ось приклад:

```Elm
str : String
str = "Хай, "

name : String
name = "Володимир"

greeting : String
greeting = str ++ name
```
У вихідних даних ми будемо мати: `Хай, Володимир`

## Історія та альтернативи:
Інтерполяція рядків - це загальноприйнята властивість в багатьох мовах програмування. В Elm ця робота виконується через використання оператора конкатенації `++`. Незважаючи на те, що Elm не має вбудованого строкового форматування, це робить Elm більш чистим та простим в настройці. Як альтернатива, ви можете використовувати бібліотеку `elm-format-string`, яка надає функції форматування строк.

## Що ще подивитись:
1. Документація Elm: [https://elm-lang.org/docs](https://elm-lang.org/docs)
2. Інтерполяція рядків в Elm - Дискусія: [https://discourse.elm-lang.org/t/string-interpolation/524](https://discourse.elm-lang.org/t/string-interpolation/524)
3. `elm-format-string` на GitHub: [https://github.com/zwilias/elm-format-string](https://github.com/zwilias/elm-format-string)