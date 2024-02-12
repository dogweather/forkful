---
title:                "Використання регулярних виразів"
aliases:
- /uk/haskell/using-regular-expressions.md
date:                  2024-02-03T19:17:21.514056-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Регулярні вирази у програмуванні – це послідовності символів, що визначають шаблон пошуку, які, як правило, використовуються для пошуку та маніпуляції з рядками. Програмісти Haskell використовують регулярні вирази для завдань, що варіюються від простого пошуку рядків до складної обробки тексту, використовуючи їх ефективність та універсальність при роботі з текстовими даними.

## Як:
У Haskell функціональності регулярних виразів не є частиною стандартної бібліотеки, що потребує використання сторонніх пакетів, таких як `regex-base` разом зі сумісним backend, наприклад `regex-posix` (для підтримки POSIX регулярних виразів), `regex-pcre` (для сумісності з регулярними виразами Perl) тощо. Ось як ви можете використовувати ці пакети для роботи з регулярними виразами.

Спочатку, переконайтеся, що у вас встановлені пакети, додавши `regex-posix` або `regex-pcre` до файлу `.cabal` вашого проекту або встановивши їх безпосередньо через cabal:

```bash
cabal install regex-posix
```
або
```bash
cabal install regex-pcre
```

### Використання `regex-posix`:

```haskell
import Text.Regex.Posix ((=~))

-- Перевірка, чи рядок відповідає шаблону
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- Знаходження першого співпадіння
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- Вивід: True
    print $ findFirst "good morning, good night" "good"
    -- Вивід: "good"
```

### Використання `regex-pcre`:

```haskell
import Text.Regex.PCRE ((=~))

-- Знаходження всіх співпадінь
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Вивід: ["test1","test2","test3"]
```

Кожна бібліотека має свої особливості, але загальна методологія використання `=~` для застосування регулярного виразу залишається послідовною, незалежно від того, чи перевіряєте ви на співпадіння, чи витягуєте підрядки. Вибір між `regex-posix` або `regex-pcre` значною мірою залежить від потреб вашого проекту та специфічних можливостей регулярних виразів, які вам необхідні.
