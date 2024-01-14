---
title:    "Haskell: Читання текстового файлу"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Чому

У цій статті ми дізнаємося, як читати текстовий файл у мові програмування Haskell та яку користь це може мати.

# Як це зробити

Для того, щоб прочитати текстовий файл у Haskell, ми можемо використовувати стандартну бібліотеку `Text`. Для початку, необхідно імпортувати модуль `System.IO`, який містить необхідні функції для роботи з файлами.

```Haskell
import System.IO
```

Далі необхідно відкрити файл за допомогою функції `openFile`, яка приймає два аргументи: шлях до файлу та режим відкриття.

```Haskell
fileHandle <- openFile "file.txt" ReadMode
```

Отриманий змінний `fileHandle` містить вказівник на відкритий файл. За допомогою функції `hGetContents` ми можемо прочитати вміст файлу у вигляді рядка.

```Haskell
fileContents <- hGetContents fileHandle
```

І на останок, закриваємо файл за допомогою функції `hClose`.

```Haskell
hClose fileHandle
```

Тепер у змінній `fileContents` ми маємо рядок з вмістом текстового файлу, який можемо розбирати та обробляти.

# Глибокий занурення

Також важливо знати, що в мові Haskell є багато інструментів для роботи з текстовими файлами. Якщо вам потрібно прочитати вміст файлу построково, ви можете скористатися функцією `lines`, яка розбиває текст на рядки за символом переносу рядка.

```Haskell
fileLines <- fmap lines (readfile "file.txt")
```

Також можна обробляти текст за допомогою регулярних виразів за допомогою модуля `Text.Regex`. Тут детальніше про роботу з регулярними виразами у Haskell: [посилання](https://www.reddit.com/r/haskell/comments/84jo0h/beginners_guide_to_regexes_in_haskell/).

# Дивись також

- [Офіційна документація з роботи з файлами у Haskell](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-IO.html)
- [Туторіал з роботи з файлами у Haskell](https://www.codewars.com/kata/51c8e184a9d15af5a8000055)