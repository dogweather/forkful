---
title:                "Робота з YAML"
html_title:           "Arduino: Робота з YAML"
simple_title:         "Робота з YAML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
YAML — це формат серіалізації даних, людино-читабельний, ідеальний для конфігураційних файлів. Програмісти використовують YAML для простоти створення та розбору даних без втрати читабельності.

## Як це зробити:
Elm поки що не має вбудованої підтримки для роботи з YAML. Втім, можна взаємодіяти з YAML через JavaScript, використовуючи порти.

```Elm
port module Main exposing (..)

-- Визначаємо порт для надсилання YAML даних на JavaScript
port toJs : String -> Cmd msg

-- Визначаємо порт для отримання даних з JavaScript
port fromJs : (String -> msg) -> Sub msg

-- Приклад використання портів
main = ...

-- Подія відправки YAML у JavaScript
sendYaml : String -> Cmd msg
sendYaml yamlData = toJs yamlData

-- Слухання відповідей з JavaScript на YAML зміни
listenYaml : (String -> msg) -> Sub msg
listenYaml handler = fromJs handler
```
Важливо пам'ятати, у вашому JavaScript файлі ви мусите налаштувати прослуховування Elm портів та визначити логіку конвертації YAML у JSON і навпаки.

## Поглиблено:
YAML (YAML Ain't Markup Language) з'явився у 2001 році. Він простіший за XML та JSON для ручного створення та редагування. Розробники воліють YAML для конфігурацій через його чистий синтаксис. Незважаючи на відсутність вбудованої підтримки в Elm, YAML може бути використаний через JavaScript порти. Альтернативою може бути JSON, який має пряму підтримку в Elm.

## Більше інформації:
- YAML офіційний сайт: [https://yaml.org](https://yaml.org)
- Про порти в Elm: [https://guide.elm-lang.org/interop/ports.html](https://guide.elm-lang.org/interop/ports.html)
