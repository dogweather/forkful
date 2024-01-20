---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Парсинг дати з рядка - це техніка, яка дозволяє комп'ютеру розпізнати дату в заданому текстовому форматі. Це важливо для успішного аналізу даних, сортування та фільтрації.

## Як це робити?
Тут приведений простий приклад парсингу дати в Haskell:

```Haskell
import Data.Time

parseDate :: String -> IO Day
parseDate = parseTimeM True defaultTimeLocale "%d-%m-%Y" 
  
main = print =<< parseDate "22-12-2020"
```
Виходом є:
```Haskell
2020-12-22
```
Функція `parseTimeM` змінює рядок у тип `Day`, використовуючи прийняті об'єкти часу та місцевості.

## Поглиблено
Бібліотека `Data.Time` в Haskell була розроблена в рамках пакету time, який був представлений у 2006 році і є частиною базового репозиторію GHC з тих пір. Він надає можливості для роботи з часом та датами.

Існують альтернативні пакети для парсингу дати як, наприклад, `Data.Time.Format.Parse`, який дає більший контроль над форматом дати.

Специфіка реалізації полягає в тому, що при розборі дати, Haskell використовує монади для обробки помилок. Якщо формат дати не вгадується, Haskell поверне помилку.

## Дивіться також
Для більш детального ознайомлення з темою, перегляньте наступні ресурси:

1. [Документація `Data.Time`](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
2. [Проект Haskell Time на GitHub](https://github.com/haskell/time)
3. [Офіційний учбовий посібник Haskell](https://www.haskell.org/tutorial/)