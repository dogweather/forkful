---
title:                "Видалення символів, що відповідають патерну"
html_title:           "C: Видалення символів, що відповідають патерну"
simple_title:         "Видалення символів, що відповідають патерну"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Що і чому?
Видалення символів, які відповідають паттерну - це процес, при якому з рядка усуваються певні символи, згідно з заданим шаблоном. Програмісти це роблять, щоб маніпулювати даними, очищуючи їх від непотрібної інформації.

## Як це робити:
```Haskell
import Data.Char
import Data.List

deleteMatchingChars :: String -> String -> String
deleteMatchingChars pattern = filter (`notElem` pattern)

main = do
  print $ deleteMatchingChars "ao" "hello world"
```
У цьому прикладі ми видаляємо символи 'a' і 'o' з рядка "hello world". Результат буде "hell wrld".

## Занурення у тему
Історично, операції над рядками були важливою складовою програмування. У Haskell підхід до роботи з рядками спрощений, але все ж потужний. Ще одним способом видалення символів є використання функції delete, але вона видаляє лише один екземпляр символу. Наступним кроком може бути створення більш складного паттерну, який використовує регулярні вирази. Пам'ятайте, на Haskell важливо розуміти час виконання та кількість пам'яті, яку використовує ваше рішення.

## Додатково
2. Real World Haskell: [Розділ про рядки та символи](http://book.realworldhaskell.org/read/efficient-file-processing-regular-expressions-and-file-name-matching.html)
3. Haskell Guidelines: [Рекомендації щодо обробки рядків](https://wiki.haskell.org/Performance/Strings)