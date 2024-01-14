---
title:                "Haskell: Видобування підрядків"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Чому
Розбивання рядків на підрядки є важливим елементом багатьох програм на мові Haskell. Це дозволяє отримати певну частину рядка, змінити її або просто використовувати як окремий елемент даних.

## Як це зробити
Для початку, ми повинні імпортувати модуль "Data.String" для роботи з рядками. Далі, є кілька методів для отримання підрядків:

```Haskell
-- Задаємо рядок
let str = "Привіт, світ!"
-- Завдяки функції "take" можемо взяти перші 6 символів
take 6 str -- Виведе "Привіт"
-- Функція "drop" дозволяє викинути перші 7 символів
drop 7 str -- Виведе "світ!"
-- Щоб взяти середню частину рядка, можна скористатися функцією "takeWhile"
takeWhile (\c -> c /= ',') str -- Виведе "Привіт"
```

## Глибокий занурення
У модулі "Data.String" також є багато інших функцій для роботи з підрядками, таких як "replace", "splitOn" і "lines". Також, можна використовувати регулярні вирази через модуль "Text.Regex". Все це дозволяє більш гнучко та ефективно працювати з рядками у програмах на Haskell.

# Дивіться також
- [Документація по модулю Data.String](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-String.html)
- [Приклади використання модуля Data.String](https://www.codewars.com/kata/569b5cec9e362e61aa00016d)
- [Варіанти роботи з рядками через регулярні вирази](https://www.haskell.org/haskellwiki/Regular_expressions)