---
title:    "Haskell: Створення випадкових чисел"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Чому

Згенерувати випадкові числа - це потрібно для створення програм, які потребують випадкових значень, наприклад, ігор або статистичних аналізів.

## Як це зробити

```Haskell
import System.Random -- Імпортуємо модуль
myRandomGen :: Int -> Int -> [Int] -- Функція для згенерування списку випадкових чисел заданої довжини з діапазону
myRandomGen n range = take n $ randomRs (1, range) (mkStdGen 42) -- Першим параметром вказуємо кількість чисел, а другим - максимальне значення 

main :: IO () -- Функція для виведення результату на екран
main = do 
    print "Ось список випадкових чисел:" 
    putStrLn (show (myRandomGen 10 100)) -- Викликаємо функцію та задаємо бажану довжину списку та діапазон чисел 
```

Вивід:

```Haskell
[13,62,53,3,8,80,15,16,33,65]
```

## Глибокий занурення

Функція `randomRs` із модуля `System.Random` дозволяє генерувати випадкові числа з будь-якими діапазонами та з використанням параметрів, що дозволяє контролювати генерування чисел. Також, використовуючи функцію `mkStdGen` можна вказати конкретне значення для початкового генератора випадкових чисел.

## Дивіться також

- [Документація Haskell про згенеровані випадкові числа](https://hackage.haskell.org/package/random-1.2.0.0/docs/System-Random.html)
- [Туторіал про генерацію випадкових чисел в Haskell](https://haskell-explained.gitlab.io/blog/posts/2017/11/22/random-numbers-in-haskell/)