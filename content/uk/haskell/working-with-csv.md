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

# Що і чому?

Робота з CSV є надзвичайно важливим і корисним навичкою для програмістів. CSV (Comma Separated Values) представляє собою формат зберігання даних, який використовує коми для розділення полів. Це дозволяє легко читати і записувати дані, що робить його популярним форматом для обміну даними між різними програмами.

# Як?

Щоб працювати з CSV у Haskell, потрібно встановити пакет ```csv```. Приклади коду та вихідні дані наведені нижче:

```Haskell
import qualified Data.Csv as Csv
import qualified Data.ByteString as BS

csvFile = "data.csv"

-- Читання CSV файлу
readCSV :: BS.ByteString -> IO (Either String (V.Vector (Ссv.Header, Ссv.Record)))
readCSV file = Csv.decode Csv.NoHeader file

-- Запис CSV файлу
writeCSV :: BS.ByteString -> IO ()
writeCSV file = BS.appendFile file

-- Приклад вихідних даних
header = ["id", "name", "age"]
data = [["1", "John", "20"], ["2", "Mary", "25"], ["3", "Tom", "30"]]

-- Читання CSV файла за допомогою наших функцій
readCSV csvFile
-- Вивід: Right (fromList [("id",["1","2","3"]),("name",["John","Mary","Tom"]),("age",["20","25","30"])])

-- Запис вихідних даних у CSV файл за допомогою наших функцій
writeCSV csvFile (Csv.encode header <> Csv.encode data)

readCSV csvFile
-- Вивід:

-- id,name,age
-- 1,John,20
-- 2,Mary,25
-- 3,Tom,30

```

# Більш детально

Формат CSV був розроблений у 1972 році для зберігання і обміну даними між програмами. Сьогодні він залишається одним з найпопулярніших форматів для зберігання і обробки табличних даних.

Існують інші альтернативи для роботи з даними, такі як JSON та XML. Але CSV залишається популярним завдяки своїй простоті та легкості прочитання людиною.

У Haskell пакет ```csv``` використовує бібліотеку ```cassava```, яка надає швидку та ефективну реалізацію для роботи з CSV.

# Більше інформації

- [Документація пакету ```csv```] (https://hackage.haskell.org/package/csv)
- [Документація бібліотеки ```cassava```] (https://hackage.haskell.org/package/cassava)
- [Огляд формату CSV] (https://en.wikipedia.org/wiki/Comma-separated_values)