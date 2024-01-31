---
title:                "Робота з CSV файлами"
date:                  2024-01-19
html_title:           "Arduino: Робота з CSV файлами"
simple_title:         "Робота з CSV файлами"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Що & Чому?
Робота з CSV (Comma-Separated Values) – це операції з текстовими файлами, які містять дані, розділені комами. Програмісти використовують CSV для простої передачі табличних даних між різними програмами.

## Як це робити:
```Haskell
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv

type Person = (String, Int, String)

-- Зчитуємо CSV-файл
readCsv :: FilePath -> IO (Either String (V.Vector Person))
readCsv filePath = do
    csvData <- BL.readFile filePath
    return $ decode NoHeader csvData

-- Записуємо CSV-файл
writeCsv :: FilePath -> V.Vector Person -> IO ()
writeCsv filePath dataToWrite = BL.writeFile filePath (encode dataToWrite)

main :: IO ()
main = do
  -- Читаємо
  persons <- readCsv "people.csv"
  case persons of
    Left err -> putStrLn err
    Right ps -> print ps

  -- Пишемо
  let newPersons = V.fromList [("Alice", 30, "Doctor"), ("Bob", 22, "Engineer")]
  writeCsv "new_people.csv" newPersons
```
### Вивід
```
[("John Doe", 43, "Software Developer"), ("Jane Smith", 29, "Data Scientist")]
```

## Поглиблений розбір
CSV файл - це старий і універсальний формат обміну даними. Історично він зарекомендував себе через свою простоту і підтримку багатьма програмами. Як альтернативи існують JSON, XML та інші формати; кожен має свої переваги. У Haskell робота з CSV може використовувати пакет `cassava`, який надає потужні засоби серіалізації та десеріалізації CSV.

## Дивіться також
- [Hackage `cassava` package](https://hackage.haskell.org/package/cassava)
- [Haskell Wiki CSV page](https://wiki.haskell.org/CSV)
- [Stack Overflow topics on Haskell CSV handling](https://stackoverflow.com/questions/tagged/csv+haskell)
