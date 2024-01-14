---
title:                "Haskell: Видалення символів, що відповідають шаблону"
simple_title:         "Видалення символів, що відповідають шаблону"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому

Зазвичай, коли працюємо зі стрічками у функціональній мові програмування Haskell, часом нам потрібно видаляти символи, які відповідають певному шаблону. Це може бути корисно, наприклад, для очищення даних або фільтрування потрібних даних.

## Як це зробити

### З використанням функції *filter*

```Haskell
-- Видаляє всі літери, що повторюються

removeDuplicates :: String -> String
removeDuplicates str = filter (not . `elem` ['a'..'z']) str

main = do
  let str = "abbaccadd"
  putStrLn $ removeDuplicates str -- виведе "bc"
```

### З використанням функції *delete*

```Haskell
-- Видаляє всі голосні зі стрічки

removeVowels :: String -> String
removeVowels str = delete 'a' (delete 'e' (delete 'i' (delete 'o' (delete 'u' str))))

main = do
  let str = "Hello world"
  putStrLn $ removeVowels str -- виведе "Hll wrld"
```

## Глибоке вивчення

У Haskell існує багато додаткових функцій для роботи зі стрічками, які можуть бути корисними для видалення символів за певним шаблоном. Наприклад, функція *deleteFirstsBy* дозволяє видаляти символи за певним критерієм.

## Дивись також

- [Функція *filter* в Haskell](https://wiki.haskell.org/Filter)
- [Функція *delete* в Haskell](https://www.geeksforgeeks.org/haskell-delete-function/)
- [Повний список функцій для роботи зі стрічками в Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-String.html)