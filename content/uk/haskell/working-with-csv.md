---
title:                "Haskell: Робота з CSV"
simple_title:         "Робота з CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Найпростішим способом роботи зі структурованими даними, такими як таблиці чи бази даних, є робота з форматом CSV. Він дозволяє зберігати дані у зручному для обробки текстовому форматі та легко зчитувати їх у будь-якій програмі. У цій статті ми дізнаємося, як працювати з CSV у Haskell.

## Як

Для початку, нам знадобиться імпортувати модуль `Data.CSV`. Він містить усі необхідні типи та функції для роботи з CSV.

```Haskell
import Data.CSV
```

Для початку, давайте створимо простий CSV-файл за допомогою функції `saveCSV`:

```Haskell
let records = [["Name", "Age"], ["John", "25"], ["Mary", "30"]]
saveCSV "my_data.csv" records
```

Також ми можемо зчитати дані із CSV-файлу за допомогою функції `loadCSV`:

```Haskell
csvData <- loadCSV "my_data.csv"
```

Тепер ми маємо зчитані дані у вигляді `[[Field]]`, де кожен елемент це рядок CSV-файлу. Ми можемо перетворити ці дані у більш зручну структуру, наприклад, у список записів:

```Haskell
let records = map recordToPerson csvData
where recordToPerson [name, age] = Person name (read age)
```

Тепер залишилося тільки зробити функцію `recordToPerson`, яка перетворює список полів у структуру `Person`, а також описати тип даних `Person`:

```Haskell
data Person = Person { name :: String, age :: Int }
    deriving Show
```

Готово, тепер ми можемо зберегти дані у вигляді списку записів у CSV-файл, використовуючи функцію `saveCSV`.

У Haskell також є багато інших цікавих функцій для роботи з CSV, таких як `parseCSV`, `printCSV`, `encodeCSV` та інші. Для більш детальної інформації, перегляньте документацію до модуля `Data.CSV`.

## Глибокий занурення

Розглянемо дещо складніший приклад. Допустимо, ми маємо CSV-файл з даними про ринкову ціну акцій, у форматі `Symbol, Date, Open, High, Low, Close`. Нам потрібно зчитати ці дані та знайти максимальну ціну за кожен символ.

Спершу, зчитаємо файл та перетворимо його у список записів:

```Haskell
csvData <- loadCSV "stocks.csv"
let records = map recordToStock csvData
    where recordToStock [symbol, date, open, high, low, close] = Stock symbol date (read open) (read high) (read low) (read close)
```

Тепер ми можемо використати функцію `groupWith` з модуля `Data.List` для групування записів за символом та знаходження максималь