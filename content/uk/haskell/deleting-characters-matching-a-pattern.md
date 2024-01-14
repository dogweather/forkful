---
title:                "Haskell: Видалення символів, відповідних патерну"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Чому
У вашому коді можуть бути непотрібні символи, які потрібно видалити, щоб програма працювала правильно. У цьому пості я покажу, як видалити символи за вказаним шаблоном у мові Haskell.

## Як це зробити
```Haskell
deletePattern :: String -> String -> String
deletePattern pattern str = filter (`notElem` pattern) str

main :: IO ()
main = do
  let myString = "Hello world!!!"
  let result = deletePattern "!l" myString
  print result
```

В цьому коді ми створюємо функцію `deletePattern`, яка приймає два аргументи - шаблон для видалення і рядок, з якого видалити символи. За допомогою функції `filter` ми проходимо через кожен символ рядка і, якщо він не належить шаблону, додаємо його до нового рядка. У показаному прикладі ми видаляємо всі символи "!l" і отримуємо на виході рядок "Heo word".

## Глибоке занурення
Цей підхід до видалення символів за шаблоном може бути корисним в різних ситуаціях. Наприклад, ви можете використовувати його для очищення даних від ненужних символів перед обробкою або для фільтрації користувальницького вводу. Крім того, ви можете змінити функцію `deletePattern`, щоб вона приймала шаблон у вигляді регулярного виразу для більш гнучкого видалення символів.

## Дивись також
- [Офіційна документація Haskell](https://www.haskell.org/documentation/)
- [Регулярні вирази в Haskell](https://www.youtube.com/watch?v=tjzLdU4P0EY)
- [Функція `filter` в Haskell](https://www.geeksforgeeks.org/haskell-filter-function/)