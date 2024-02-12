---
title:                "Використання регулярних виразів"
aliases:
- /uk/elm/using-regular-expressions.md
date:                  2024-02-03T19:16:58.763194-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Регулярні вирази (регекс) у програмуванні - це шаблони, які використовуються для пошуку комбінацій символів у рядках. У Elm, як і в інших мовах, програмісти використовують регекси для задач, таких як перевірка вводу, пошук та заміна тексту в рядках, через їхню гнучкість та ефективність.

## Як:
Elm не має вбудованих функцій регекс у своїй основній бібліотеці, що вимагає використання сторонніх бібліотек для цих операцій. Одним із популярних виборів для роботи з регексами є `elm/regex`. Ви можете додати його до свого проекту, використовуючи `elm install elm/regex`.

Ось як ви можете використовувати `elm/regex` для кількох поширених задач:

### 1. Знаходження відповідності до шаблону
Для перевірки, чи відповідає рядок шаблону, ви можете використовувати `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Приклад використання:
isAlphanumeric "Elm2023"     -- Вивід: True
isAlphanumeric "Elm 2023!"   -- Вивід: False
```

### 2. Пошук усіх відповідностей
Для того, щоб знайти усі випадки шаблону в рядку, ви можете використовувати `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Приклад використання:
getWords "Elm is fun!"  -- Вивід: ["Elm", "is", "fun"]
```

### 3. Заміна тексту
Для заміни частин рядка, які відповідають шаблону, ви використовуєте `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Приклад використання:
replaceElmWithHaskell "Learning Elm is fun!"  
-- Вивід: "Навчання Haskell весело!"
```

У цих прикладах `Regex.fromString` використовується для компіляції шаблону регексу, де `\b` відповідає межам слів, а `\w` - будь-якому символу слова. Завжди обробляйте результат `Maybe` від `Regex.fromString`, щоб захиститися від недійсних шаблонів регексу, зазвичай використовуючи `Maybe.withDefault`.
