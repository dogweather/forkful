---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:58.763194-07:00
description: "\u042F\u043A: Elm \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u0438\u0445 \u0444\u0443\u043D\u043A\u0446\u0456\u0439\
  \ \u0440\u0435\u0433\u0435\u043A\u0441 \u0443 \u0441\u0432\u043E\u0457\u0439 \u043E\
  \u0441\u043D\u043E\u0432\u043D\u0456\u0439 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u0446\u0456, \u0449\u043E \u0432\u0438\u043C\u0430\u0433\u0430\u0454 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0441\u0442\u043E\
  \u0440\u043E\u043D\u043D\u0456\u0445 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A \u0434\u043B\u044F \u0446\u0438\u0445 \u043E\u043F\u0435\u0440\u0430\u0446\
  \u0456\u0439. \u041E\u0434\u043D\u0438\u043C \u0456\u0437 \u043F\u043E\u043F\u0443\
  \u043B\u044F\u0440\u043D\u0438\u0445\u2026"
lastmod: '2024-03-13T22:44:49.134364-06:00'
model: gpt-4-0125-preview
summary: "Elm \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u0438\u0445 \u0444\u0443\u043D\u043A\u0446\u0456\u0439 \u0440\u0435\
  \u0433\u0435\u043A\u0441 \u0443 \u0441\u0432\u043E\u0457\u0439 \u043E\u0441\u043D\
  \u043E\u0432\u043D\u0456\u0439 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u0446\
  \u0456, \u0449\u043E \u0432\u0438\u043C\u0430\u0433\u0430\u0454 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0441\u0442\u043E\u0440\u043E\
  \u043D\u043D\u0456\u0445 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\
  \ \u0434\u043B\u044F \u0446\u0438\u0445 \u043E\u043F\u0435\u0440\u0430\u0446\u0456\
  \u0439."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

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
