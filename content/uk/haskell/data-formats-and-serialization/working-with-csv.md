---
title:                "Робота з CSV"
date:                  2024-02-03T19:20:07.901563-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з CSV-файлами (значення, розділені комами), передбачає парсинг та створення файлів, що зберігають табличні дані у простому текстовому форматі. Програмісти часто займаються цим завданням, щоб ефективно імпортувати або експортувати дані з електронних таблиць, баз даних, або для спрощення обміну даними між різними програмами.

## Як це зробити:

У Haskell робота з CSV-файлами можлива за допомогою бібліотеки `cassava`, однієї з популярних сторонніх бібліотек для цієї мети. Нижче наведено приклади, які демонструють, як читати з CSV-файлів і записувати в них, використовуючи `cassava`.

**1. Читання з CSV-файлу:**

Спершу переконайтеся, що у вас встановлено `cassava`, додавши її до файлу cabal вашого проекту або використовуючи Stack.

Ось простий приклад читання CSV-файлу та виведення кожного запису. Припускаємо, що у CSV-файлі є дві колонки: ім'я та вік.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " має " ++ show (age :: Int) ++ " років."
```

Припустимо, `people.csv` містить:
```
John,30
Jane,25
```
Вивід буде:
```
John має 30 років.
Jane має 25 років.
```

**2. Запис у CSV-файл:**

Щоб створити CSV-файл, можна використати функцію `encode` з `cassava`.

Ось як ви могли б записати список записів у CSV-файл:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Після запуску цієї програми, `output.csv` буде містити:

```
John,30
Jane,25
```

Це стислий вступ до роботи з CSV-файлами у Haskell за допомогою бібліотеки `cassava` демонструє, як читати з CSV-файлів і записувати в них, роблячи завдання маніпуляції даними більш доступними для тих, хто тільки починає працювати з цією мовою.
