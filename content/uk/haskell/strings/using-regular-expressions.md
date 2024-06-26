---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:21.514056-07:00
description: "\u042F\u043A: \u0423 Haskell \u0444\u0443\u043D\u043A\u0446\u0456\u043E\
  \u043D\u0430\u043B\u044C\u043D\u043E\u0441\u0442\u0456 \u0440\u0435\u0433\u0443\u043B\
  \u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\u0456\u0432 \u043D\
  \u0435 \u0454 \u0447\u0430\u0441\u0442\u0438\u043D\u043E\u044E \u0441\u0442\u0430\
  \u043D\u0434\u0430\u0440\u0442\u043D\u043E\u0457 \u0431\u0456\u0431\u043B\u0456\u043E\
  \u0442\u0435\u043A\u0438, \u0449\u043E \u043F\u043E\u0442\u0440\u0435\u0431\u0443\
  \u0454 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F\
  \ \u0441\u0442\u043E\u0440\u043E\u043D\u043D\u0456\u0445 \u043F\u0430\u043A\u0435\
  \u0442\u0456\u0432, \u0442\u0430\u043A\u0438\u0445 \u044F\u043A `regex-base`\u2026"
lastmod: '2024-03-13T22:44:49.340969-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Haskell \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\u043B\
  \u044C\u043D\u043E\u0441\u0442\u0456 \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\
  \u0438\u0445 \u0432\u0438\u0440\u0430\u0437\u0456\u0432 \u043D\u0435 \u0454 \u0447\
  \u0430\u0441\u0442\u0438\u043D\u043E\u044E \u0441\u0442\u0430\u043D\u0434\u0430\u0440\
  \u0442\u043D\u043E\u0457 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438\
  , \u0449\u043E \u043F\u043E\u0442\u0440\u0435\u0431\u0443\u0454 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0441\u0442\u043E\u0440\u043E\
  \u043D\u043D\u0456\u0445 \u043F\u0430\u043A\u0435\u0442\u0456\u0432, \u0442\u0430\
  \u043A\u0438\u0445 \u044F\u043A `regex-base` \u0440\u0430\u0437\u043E\u043C \u0437\
  \u0456 \u0441\u0443\u043C\u0456\u0441\u043D\u0438\u043C backend, \u043D\u0430\u043F\
  \u0440\u0438\u043A\u043B\u0430\u0434 `regex-posix` (\u0434\u043B\u044F \u043F\u0456\
  \u0434\u0442\u0440\u0438\u043C\u043A\u0438 POSIX \u0440\u0435\u0433\u0443\u043B\u044F\
  \u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\u0456\u0432), `regex-pcre`\
  \ (\u0434\u043B\u044F \u0441\u0443\u043C\u0456\u0441\u043D\u043E\u0441\u0442\u0456\
  \ \u0437 \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u043C\u0438 \u0432\
  \u0438\u0440\u0430\u0437\u0430\u043C\u0438 Perl) \u0442\u043E\u0449\u043E."
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

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
