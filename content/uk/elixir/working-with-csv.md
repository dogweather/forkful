---
title:                "Робота з csv"
html_title:           "Elixir: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & З чого почати?
Робота з CSV - це спосіб збереження табличних даних у вигляді текстового файлу. Цей формат є дуже поширеним серед програмістів, оскільки дозволяє легко обмінюватися даними між різними програмами та платформами.

## Як це робити?
```Elixir 
require File
CSV.read("file.csv") 
```

Цей код дозволить вам прочитати CSV файл та зберегти дані у вигляді списків. Також можна використовувати функції ```CSV.parse``` та ```CSV.encode``` для роботи зі змістом файлу та збереження його у потрібному форматі.

## Поглиблене дослідження
Історично, CSV був створений для взаємодії з електронними таблицями, але з часом став популярним серед розробників програмного забезпечення в усіх галузях. Існують також інші формати для збереження табличних даних, такі як JSON та XML, але CSV є дуже простим та легким у використанні.

Над реалізацією роботи з CSV в Elixir працює велика спільнота, тому ви можете легко знайти документацію та приклади використання.

## Дивіться також
- [Elixir документація для CSV](https://hexdocs.pm/csv/CSV.html)
- [Порівняння з форматом JSON](https://stackoverflow.com/questions/2044583/csv-versus-json-structured-versus-flat-files)
- [Порівняння з форматом XML](https://www.sitepoint.com/parsing-csv-with-elixir/)