---
title:                "Робота з CSV"
aliases:
- uk/elm/working-with-csv.md
date:                  2024-02-03T19:19:35.409307-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з CSV (Comma Separated Values, значення, розділені комами) передбачає аналіз та генерування файлів, що зберігають табличні дані у простому текстовому форматі. Це поширена практика серед програмістів для забезпечення легкого обміну даними між різними додатками або для ефективної обробки великих наборів даних у типізований спосіб в Elm.

## Як це робити:

Elm не має вбудованої підтримки для аналізу або генерації CSV; натомість часто використовуються сторонні пакети, наприклад, `panosoft/elm-csv`. Нижче наведені приклади базового використання цієї бібліотеки для аналізу та генерації CSV.

### Аналіз CSV

Спочатку вам потрібно додати пакет CSV до вашого проекту Elm:

```bash
elm install panosoft/elm-csv
```

Потім ви можете аналізувати рядок CSV у список записів. Простий приклад:

```elm
import Csv

csvData : String
csvData =
   "name,age\nJohn Doe,30\nJane Smith,25"

parseResult : Result String (List (List String))
parseResult =
    Csv.parse csvData

-- Приклад виводу: Ok [["name","age"],["John Doe","30"],["Jane Smith","25"]]
```

### Генерація CSV

Для генерації рядка CSV з даних Elm використовуйте функцію `Csv.encode`:

```elm
import Csv

records : List (List String)
records =
    [ ["name", "age"]
    , ["John Doe", "30"]
    , ["Jane Smith", "25"]
    ]

csvOutput : String
csvOutput =
    Csv.encode records

-- Приклад виводу: "name,age\nJohn Doe,30\nJane Smith,25\n"
```

Цей спрощений підхід дозволяє інтегрувати функціональність CSV у ваші додатки Elm, використовуючи типізоване середовище для маніпуляції даними та обміну.
