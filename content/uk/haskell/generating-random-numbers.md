---
title:                "Haskell: Створення випадкових чисел"
simple_title:         "Створення випадкових чисел"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому

Потрібно генерувати випадкові числа для багатьох різних завдань, таких як шифрування, моделювання та аналіз даних.

## Як

Найпростішим способом генерувати випадкові числа в Haskell є використання функції `random` з модуля `System.Random`. Вона приймає діапазон чисел і повертає випадкове число з цього діапазону.

```Haskell
import System.Random

main = do
    -- генеруємо випадкове число від 1 до 10
    randomInt <- randomRIO (1,10)
    putStrLn ("Ваше випадкове число: " ++ show randomInt)
```

В результаті отримаємо випадкове число від 1 до 10, яке буде виведене на екран.

```
Ваше випадкове число: 7
```

## Глибше

Основний принцип генерування випадкових чисел - це використання алгоритмів, які використовують початкове значення (зіткнення) для генерування наступних чисел у певному порядку. У Haskell є багато різних алгоритмів генерування випадкових чисел, таких як `System.Random.Mersenne`, `System.Random.SplitMix`, `System.Random.PCG`. Кожен з них має свої переваги та недоліки, і вибір підходящого алгоритму залежить від потреб програми.

## Дивись також

- [Документація по модулю `System.Random`](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Порівняння різних алгоритмів генерування випадкових чисел у Haskell](https://wiki.haskell.org/Random_number_generation)