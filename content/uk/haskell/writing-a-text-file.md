---
title:                "Haskell: Написання текстового файлу"
simple_title:         "Написання текстового файлу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Чому:

Написання текстових файлів є важливою частиною багатьох програмних проектів. Воно дозволяє зберігати та передавати дані між різними програмами, спрощуючи роботу з великими обсягами даних.

## Як:

```Haskell
main :: IO()
main = do
    let text = "Це приклад написання текстового файлу в Haskell."
    writeFile "example.txt" text -- створення та запис даних у файл
    putStrLn "Файл успішно створено!"
```

Після запуску коду, у вас буде створений новий файл під назвою "example.txt", який буде містити рядок "Це приклад написання текстового файлу в Haskell."

## Глибше погляд:

Щоб користуватися записом текстових файлів у Haskell, необхідно імпортувати модуль "System.IO". Для передачі даних від програми до файла використовується функція `writeFile`, яка приймає два параметри: шлях до файлу та дані, які потрібно записати. Якщо файл вже існує, він буде перезаписаний.

## Дивись також:

- [Основи файлової системи у Haskell](https://riptutorial.com/haskell/example/22341/basic-file-system-operations)
- [Використання модулю "System.IO" для роботи з файлами у Haskell](https://www.codementor.io/@sheharyarshahid/working-with-files-in-haskell-5l36reu6d)
- [Огляд маніпулювання файлами у Haskell](https://riptutorial.com/haskell/example/22329/file-manipulation-overview)