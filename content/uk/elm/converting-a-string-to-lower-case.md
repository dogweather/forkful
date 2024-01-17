---
title:                "Перетворення рядка на нижній регістр"
html_title:           "Elm: Перетворення рядка на нижній регістр"
simple_title:         "Перетворення рядка на нижній регістр"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?
Конвертування рядка до нижнього регістру - це процесс перетворення всіх символів у рядку на відповідні символи у нижньому регістрі. Програмісти виконують цю операцію, щоб забезпечити консистентність та однорідність даних.

## Як це зробити:
```Elm
import String

String.toLower "ELM" -- "elm"
String.toLower "HeLLo WoRld" -- "hello world"
```

## Подробиці:
Конвертування рядка до нижнього регістру стало загальноприйнятою практикою в світі програмування, оскільки це зробило обробку даних більш простою та однорідною. У більшій частині мов програмування є вбудована функція для перетворення рядка в нижній регістр, наприклад, у JavaScript це функція `toLowerCase()`. Альтернативними способами конвертування рядка до нижнього регістру є використання регулярних виразів або власноруч написаної функції.

## Дивіться також:
- Elm документація з функції `String.toLower`: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Інші методи для роботи з рядками в Elm: https://guide.elm-lang.org/appendix/strings.html