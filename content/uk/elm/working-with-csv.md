---
title:                "Робота з csv"
html_title:           "Elm: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Найпростіший спосіб імпортувати, експортувати та обробляти дані в програмуванні - це використання CSV (Comma-Separated Values) файлів. Це особливо корисно для роботи з великими обсягами даних, які легко організувати та оновлювати.

## Як

Щоб розпочати роботу з CSV у Elm, спершу треба встановити модуль [joakin/elm-csv](https://package.elm-lang.org/packages/joakin/elm-csv/latest/). Після цього, можна використовувати його, імпортуючи модуль `Csv` у початку файлу.

```Elm
import Csv
```

Далі, потрібно отримати дані з CSV файлу за допомогою функції `decode` та вказати шлях до файлу. У цьому прикладі, ми отримуємо дані з файлу "data.csv".

```Elm
Csv.decode "data.csv"
```

Тепер можна обробляти дані з файлу, наприклад, вивести їх у консоль за допомогою функції `Debug.log`.

```Elm
Csv.decode "data.csv"
    |> Debug.log "Дані з CSV файлу"
```

Також, можна використовувати функції `Csv.Decode` модуля, щоб розпарсити дані з файлу та перетворити їх у тип даних, який необхідний для подальшої обробки.

## Глибокий занурення

У модулі [joakin/elm-csv](https://package.elm-lang.org/packages/joakin/elm-csv/latest/) є багато корисних функцій для роботи з CSV. Наприклад, функція `decodeWith` дозволяє вказати власний розпарсер, щоб обробити особливі типи даних.

Також, треба пам'ятати, що дані з файлу мають бути відсортовані відповідно до заголовків стовпців, інакше їх обробка може бути ускладнена.

## Дивись також

- [Офіційна документація Elm для модуля `Csv`](https://package.elm-lang.org/packages/joakin/elm-csv/latest/)
- [Приклади використання Elm для роботи з CSV](https://github.com/joakin/elm-csv/tree/master/examples)
- [Стаття про роботу з CSV у Elm на англійській мові](https://korban.net/posts/elm/2019-04-05-working-csvs-elm/)