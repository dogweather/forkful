---
title:                "Haskell: Читання текстового файлу"
simple_title:         "Читання текстового файлу"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

### Чому
 Читання текстового файлу є важливою навичкою для будь-якого програміста. Це може бути корисною задачею для вивчення нової мови програмування, або для отримання даних з файлів вже існуючого проекту.

### Як це зробити
```Haskell
main = do
  file <- readFile "file.txt"           -- відкриваємо файл для читання
  putStrLn ("File contents: " ++ file)  -- виводимо вміст файлу
```
В даному коді ми використовуємо функцію `readFile` з модуля `System.IO`, яка автоматично відкриває файл та повертає його вміст як рядок. Потім ми використовуємо функцію `putStrLn`, яка виводить передану їй строку на екран.

### Поглиблене дослідження
Вау! Як це взагалі працює? У Haskell немає поняття "файл". Замість цього, ми працюємо зі зв'язками, які називаються "дескриптори файлів". Функція `readFile` отримує дескриптор файлу та повертає звичайний зв'язок, який можна обробити за допомогою звичайних рядкових операцій.

### Дивись також
- [Official Haskell Documentation](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell по-українськи](https://haskell.in.ua/)