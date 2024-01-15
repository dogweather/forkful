---
title:                "Написання у файл стандартних помилок"
html_title:           "Haskell: Написання у файл стандартних помилок"
simple_title:         "Написання у файл стандартних помилок"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Чому

Написання до стандартного виводу помилок є важливою частиною програмування в Haskell. Це дозволяє розробникам бачити помилки та відлагоджувати свої програми.

## Як

```Haskell
{-# LANGUAGE BangPatterns #-}
import System.IO

main = do
  !errorData <- getLine -- отримати строку від користувача
  hPutStrLn stderr errorData -- вивести помилку до стандартного виводу

```
В рядку `{-# LANGUAGE BangPatterns #-}` ми вказали, щоб Haskell оцінював значення одразу, а не відкладав його обчислення.
В нашому коді ми використовуємо функцію `getLine`, щоб отримати строку від користувача, і функцію `hPutStrLn`, щоб вивести цю строку до стандартного виводу.

### Приклад виводу

Якщо ми запустимо цей приклад та введемо "Помилка у коді", то у стандартному виводі з'явиться повідомлення "Помилка у коді".

## Глибока інформація

Крім функцій `hPutStrLn` і `getLine`, є ще багато інших функцій, які допомагають виводити до стандартного виводу та отримувати дані від користувача. Наприклад, `hPutStr` друкує строку без символів нового рядка, а `hGetLine` отримує строку зі стандартного вводу.

## Дивись також

- [Офіційна документація Haskell](https://www.haskell.org/documentation/)
- [Стаття "Вступ до Haskell"](https://uk.wikipedia.org/wiki/Haskell) на Вікіпедії
- [Курси програмування на Haskell](https://www.coursera.org/courses?query=haskell) на Coursera