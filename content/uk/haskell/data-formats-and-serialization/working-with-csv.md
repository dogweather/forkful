---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:07.901563-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Haskell \u0440\u043E\u0431\u043E\u0442\u0430 \u0437 CSV-\u0444\u0430\u0439\
  \u043B\u0430\u043C\u0438 \u043C\u043E\u0436\u043B\u0438\u0432\u0430 \u0437\u0430\
  \ \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0431\u0456\u0431\u043B\
  \u0456\u043E\u0442\u0435\u043A\u0438 `cassava`, \u043E\u0434\u043D\u0456\u0454\u0457\
  \ \u0437 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0441\u0442\
  \u043E\u0440\u043E\u043D\u043D\u0456\u0445 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\
  \u0435\u043A \u0434\u043B\u044F \u0446\u0456\u0454\u0457 \u043C\u0435\u0442\u0438\
  . \u041D\u0438\u0436\u0447\u0435\u2026"
lastmod: '2024-03-13T22:44:49.397950-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Haskell \u0440\u043E\u0431\u043E\u0442\u0430 \u0437 CSV-\u0444\u0430\
  \u0439\u043B\u0430\u043C\u0438 \u043C\u043E\u0436\u043B\u0438\u0432\u0430 \u0437\
  \u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0438 `cassava`, \u043E\u0434\u043D\u0456\u0454\
  \u0457 \u0437 \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0441\
  \u0442\u043E\u0440\u043E\u043D\u043D\u0456\u0445 \u0431\u0456\u0431\u043B\u0456\u043E\
  \u0442\u0435\u043A \u0434\u043B\u044F \u0446\u0456\u0454\u0457 \u043C\u0435\u0442\
  \u0438."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 CSV"
weight: 37
---

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
