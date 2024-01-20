---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що таке та навіщо?
Генерація випадкових чисел - це процес створення чисел, які видаються випадковими. Програмісти використовують це, щоб виконувати операції, які вимагают різного відтворення, наприклад, для ігор, криптографії, моделювання, тестування програмного забезпечення тощо.

## Як це робити:
```Haskell
import System.Random  
   
main :: IO ()  
main = do  
    gen <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen)  
```
Це простий приклад Haskell програми, яка генерує випадкову стрічку з 20 символів.

## Поглиблений розгляд
1. В історичному контексті: Генерація випадкових чисел була серйозною проблемою для інженерів та математиків у минулому. Первинними методами були використання випадкових таблиць або фізичних процесів. Тепер, з використанням комп'ютерів, це стало набагато простіше.

2. Альтернативи: Існує також можливість використовувати Pseudo Random Number Generators (PRNGs), такі як мерсенна стрибок (Mersenne Twister). Це є вибором більшості програмістів через його швидкість і ефективність.

3. Деталі реалізації: Haskell використовує стандартний генератор (StdGen), який можна отримати викликом функції getStdGen. Функція `randomRs` використовується для генерування випадкового набору символів у вказаному діапазоні.

## Див. також
- [Random Module in Haskell](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
- [StackOverflow: How do I generate a random value in Haskell?](https://stackoverflow.com/questions/30740366/how-do-i-generate-a-random-string-in-haskell)