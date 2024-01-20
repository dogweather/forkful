---
title:                "Читання текстового файлу"
html_title:           "Arduino: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Читання текстового файлу - це процес зчитування даних з файла, записаного у вигляді тексту. Програмісти роблять це, щоб маніпулювати цими даними або використовувати їх для різних цілей у своїх програмах.

## Як це зробити:

Простий код для читання файлу в Haskell:

```Haskell
import System.IO  

main = do  
    handle <- openFile "example.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle 
```

Цей код відкриє "example.txt" для зчитування, збереже його зміст у "contents" та виведе на екран.

## Поглиблений матеріал:

1. Історичний контекст: Haskell був винайдений у 1990 році з метою створення потужної мови програмування, яка спрощує читання файлів та інших операцій з введенням/виведенням.

2. Альтернативи: Є багато інших мов програмування, таких як Python або Java, які теж можна використовувати для читання текстових файлів. Вони можуть бути простішими для початківців.

3. Деталі реалізації: Функція `openFile` в Haskell використовує подвійний механізм: воно відкриває файл і повертає ідентифікатор файлу, який потім можна використовувати для зчитування змісту файлу.

## Додаткова інформація:

1. Офіційна документація Haskell: https://www.haskell.org/documentation/
2. Туторіал по читанню файлів в Haskell: http://learnyouahaskell.com/input-and-output#reading-files
3. Порівняння читання файлів в різних мовах програмування: https://www.geeksforgeeks.org/read-write-file-using-haskell/