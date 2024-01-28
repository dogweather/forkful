---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що таке & Чому?
Регулярні вирази - це шаблони для пошуку та маніпулювання текстом. Програмісти їх використовують для ефективного пошуку, перевірки, й заміни частин тексту.

## Як це робити:
```haskell
import Text.Regex.TDFA ((=~))

-- Пошук у тексті
let text = "Телефон: 01234, Дата: 2023-04-01"
let pattern = "[0-9]+"

-- Знаходження всіх збігів
matches :: String -> String -> [String]
matches text pattern = text =~ pattern

-- Приклад використання
main :: IO ()
main = print $ matches text pattern

-- Вивід: ["01234", "2023", "04", "01"]
```

## Поглиблений розгляд
Регулярні вирази з'явилися в 1950-х роках і до сьогодні залишаються важливим інструментом. Альтернативою є парсингові бібліотеки (наприклад, `Parsec` у Haskell), які надають більшу гнучкість, але зазвичай вимагають більше коду. Основою регулярних виразів в Haskell є бібліотека `regex-tdfa`, яка підтримує багато синтаксису POSIX.

## Більше Інформації
- Haskell [`regex-tdfa` documentation](https://hackage.haskell.org/package/regex-tdfa)
- [The Haskell Wiki on regular expressions](https://wiki.haskell.org/Regular_expressions)
