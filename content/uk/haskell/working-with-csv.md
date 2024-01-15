---
title:                "Робота з csv"
html_title:           "Haskell: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Якщо ви працюєте з даними, шансів свого життя вам вже доводилося працювати з CSV (Comma Separated Values) файлами. CSV є одним з найбільш поширених форматів для зберігання даних, особливо коли маєте справу з великими об'ємами даних.

## Як це зробити

Найпростіший спосіб прочитати CSV файл у Haskell - використати пакет "csv". Це можна зробити за допомогою наступного коду:

```Haskell
import Text.CSV (parseCSV)

main = do
    let csvFile = "file.csv" -- назва вашого файлу
    input <- readFile csvFile
    let csv = parseCSV csvFile input
    print csv
```
Після цього ви отримаєте дані у вигляді списку списків рядків.

Якщо потрібно обробити більш складні дані, можна використовувати функції з пакету "cassava". Наприклад, якщо у вас є CSV файл з заголовками стовпців, то за допомогою функції `decodeHeader` ви можете отримати дані у вигляді масиву об'єктів з відповідними назвами полів.

```Haskell
import Data.Csv (decodeHeader)

main = do
    let csvFile = "file.csv" -- назва вашого файлу
    input <- readFile csvFile
    case decodeHeader input of
        Left err -> putStrLn err
        Right rows -> print rows
```

## Глибоке погруження

Цей приклад є досить простим і не враховує багатьох ситуацій, які можуть виникнути під час роботи з CSV файлами. Наприклад, вам можуть знадобитися додаткові функції для парсингу конкретних типів даних або обробки помилок. Для цього можна ознайомитися з іншими пакетами, такими як "cassava" або "csv-conduit".

## Дивіться також

- [Пакет csv у Hackage](https://hackage.haskell.org/package/csv)
- [Пакет cassava у Hackage](https://hackage.haskell.org/package/cassava)
- [Пакет csv-conduit у Hackage](https://hackage.haskell.org/package/csv-conduit)