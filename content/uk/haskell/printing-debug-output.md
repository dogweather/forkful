---
title:    "Haskell: Друк відлагоджувального виводу"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

Виведення відлагоджувального виводу може бути корисним для виявлення та виправлення помилок у програмі.

## Як

```Haskell
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

main :: IO ()
main = do
  putStrLn "Введіть ціле число:"
  n <- readLn
  putStrLn ("Факторіал " ++ show n ++ " = " ++ show (factorial n))
```

В цьому прикладі ми використовуємо функцію `putStrLn` для виведення рядку на консоль, а також функцію `show` для перетворення чисел у рядковий формат.

#### Вивід:

```
Введіть ціле число:
5
Факторіал 5 = 120
```

## Глибоке занурення

Іноді для відладки коду потрібно виводити більше інформації, ніж просто значення змінних. У такому випадку можна використати функцію `trace` з модуля `Debug.Trace`:

```Haskell
import Debug.Trace (trace)

factorial :: Int -> Int
factorial n = trace ("Obtaining factorial of " ++ show n) $
             if n == 0 then 1 else n * factorial (n-1)

main :: IO ()
main = do
  putStrLn "Введіть ціле число:"
  n <- readLn
  putStrLn ("Факторіал " ++ show n ++ " = " ++ show (factorial n))
```

#### Вивід:

```
Введіть ціле число:
5
Обчислення факторіала 5
Обчислення факторіала 4
Обчислення факторіала 3
Обчислення факторіала 2
Обчислення факторіала 1
Обчислення факторіала 0
Факторіал 5 = 120
```

За допомогою функції `trace` можна виводити будь-яку інформацію, що допоможе відшукати та виправити помилки.

## Дивись також

- [Офіційна документація з моніторингу та відлагодження в Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#using-trace)
- [Відлагодження в Haskell з допомогою `Debug.Trace`](https://www.fpcomplete.com/blog/2017/01/debugging-haskell-with-debug-trace)
- [Стаття про відлагодження в Haskell на DEV Community з прикладами коду](https://dev.to/hsdaily/debugging-in-haskell-7g3)

## Подивитися також

- [Офіційна документація з введення та виведення у Haskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/io.html)
- [Туторіал з введення та виведення у Haskell на YouTube](https://www.youtube.com/watch?v=xDVC3wKjS64)
- [Стаття про маніпулювання вводом та виводом у Haskell на DEV Community з прикладами коду](https://dev.to/hsdaily/working-with-input-and-output-in-haskell-136)