---
title:                "Розбір дати з рядка"
aliases:
- /uk/elm/parsing-a-date-from-a-string/
date:                  2024-02-03T19:14:33.457089-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Аналіз дати з рядка в Elm полягає в перетворенні текстової інформації, що представляє дати та час, на формат, який Elm може розуміти та маніпулювати ним, зокрема на тип `Date`. Цей процес є критичним для обробки введення користувача, правильного локалізованого відображення дат та виконання обчислень, пов'язаних з датами, забезпечуючи інтелектуальну обробку темпоральних даних у ваших застосунках Elm.

## Як це зробити:
Elm не має вбудованих можливостей таких потужних, як у деяких інших мовах, для аналізу дат, головним чином покладаючись на взаємодію з Javascript або бібліотеки для більш складних операцій. Проте, для базового аналізу ви можете використовувати пакет `elm/time`, а для більш складних потреб широко рекомендується використовувати сторонню бібліотеку `justinmimbs/date`.

### Аналіз за допомогою `elm/time`:
`elm/time` надає модуль `Time`, який дозволяє вам працювати з мітками часу замість читабельних людиною дат. Хоча він безпосередньо не аналізує дати з рядків, ви можете перетворити рядок ISO 8601 на мітку часу POSIX, з якою потім можна працювати.

```elm
import Time exposing (Posix)

-- Припустимо, у вас є рядок дати ISO 8601
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- Перетворіть його на мітку часу POSIX (ця функція повертає `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- Приклад виводу: Ok <posix часове значення>
```

### Аналіз за допомогою `justinmimbs/date`:
Для більш складного аналізу, як наприклад обробки не ISO форматів, бібліотека `justinmimbs/date` є чудовим вибором. Ось як ви можете її використовувати для аналізу власного рядка дати:

1. Переконайтеся, що у вас встановлена ​​бібліотека:

```shell
elm install justinmimbs/date
```

2. Використовуйте функцію `Date.fromString` для аналізу власних форматів дат:

```elm
import Date
import Result exposing (Result(..))

-- Припустимо, у вас є власний формат рядка дати `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- Функція для аналізу власного формату
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- Приклад використання
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- Приклад виводу: Ok (Date.fromCalendarDate 2023 Jan 1)
```

У цих прикладах тип `Result` включає в себе або успішний аналіз, який дає дату (`Ok`), або помилку (`Err`), забезпечуючи надійне оброблення помилок у ваших застосунках Elm.
