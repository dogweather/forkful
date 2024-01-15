---
title:                "Використання регулярних виразів"
html_title:           "Elm: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Чому

Люди використовують регулярні вирази для швидкої та ефективної обробки тексту. Вони дозволяють шукати, замінювати та витягувати певні шаблони з великих обсягів даних.

## Як

```Elm
import Regex exposing (..)

myRegex : Regex
myRegex =
    regex "\\d+" -- знаходить всі числа в тексті

Regex.find myRegex "Любов - це 42" -- повертає Just (Match "42")
Regex.replace myRegex (\_ -> "1") "Любов - це 42" -- повертає "Любов - це 1"
Regex.split myRegex "Lorem ipsum dolor sit amet" -- повертає ["Lorem", "ipsum", "dolor", "sit", "amet"]
```

## Глибоке занурення

Регулярні вирази мають багато різноманітних використань, від пошуку символів до складніших шаблонів для валідації даних. Ви можете використовувати спеціальні символи, такі як '.' для будь-якого символу, '*' для повторення та '(' та ')' для групування. Для детальнішої інформації, зверніться до [документації Elm для регулярних виразів](https://package.elm-lang.org/packages/elm/regex/latest/). 

## Дивіться також

- [Курс "Введення в регулярні вирази"](https://regexone.com/)
- [Регулярні вирази для початківців](https://www.regular-expressions.info/tutorial.html)
- [Корисні інструменти для перевірки та тестування регулярних виразів](https://regex101.com/)