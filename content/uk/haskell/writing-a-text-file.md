---
title:                "Написання текстового файлу"
html_title:           "Haskell: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Написання текстового файлу є невід'ємною частиною багатьох програм, особливо в області аналізу даних та обробки текстової інформації. Використання мови Haskell дозволяє зробити цей процес більш ефективним та зручним, завдяки багатому набору функцій та дуже зрозумілому синтаксису.

## Як

Наступним чином надані два приклади коду для створення та запису текстового файлу в мові Haskell:

```
-- Приклад 1: Запис даних у файл

import System.IO

main = do
  let text = "Привіт світ!"
  writeFile "hello.txt" text
  putStrLn "Успішно записано до файлу."
```

```
-- Приклад 2: Читання даних з файлу

import System.IO

main = do
  handle <- openFile "hello.txt" ReadMode
  contents <- hGetContents handle
  putStrLn contents
  hClose handle
```

Приклад 1 використовує функцію `writeFile` для створення та запису даних у файл з назвою `hello.txt`, а приклад 2 демонструє використання функції `hGetContents` для читання вмісту файлу та його виведення на екран.

Результатом виконання цих прикладів є створення файлу `hello.txt`, який містить текст "Привіт світ!" та виведення цього самого тексту у консоль.

## Занурення

У мові Haskell є декілька вбудованих функцій для роботи з файлами, таких як `readFile`, `appendFile` та `hPutStrLn`.

Також існує багато сторонніх бібліотек, які дозволяють робити більш складні операції з файлами, такі як реалізація шаблону "почитай-перепиши" або робота з CSV-файлами.

## Дивись також

- [Офіційна документація мови Haskell](https://www.haskell.org/documentation/)
- [Ресурси для вивчення мови Haskell](https://wiki.haskell.org/Learning_Haskell)
- [Статті про роботу з файлами в Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/12-A_Simple_IO_Action)