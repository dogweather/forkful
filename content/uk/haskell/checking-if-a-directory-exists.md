---
title:                "Перевірка наявності директорії"
date:                  2024-01-20T14:57:30.422749-07:00
html_title:           "C#: Перевірка наявності директорії"
simple_title:         "Перевірка наявності директорії"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Перевірка існування директорії — це основа безпеки файлової системи у вашому коді. Програмісти роблять це, щоб запобігти помилкам під час читання чи запису файлів.

## How to: (Як робити:)
Haskell має вбудовані інструменти для роботи з файловою системою. Використовуйте модуль `System.Directory` для перевірки наявності директорій.

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    let dir = "/path/to/directory"
    dirExists <- doesDirectoryExist dir
    putStrLn $ "Directory " ++ (if dirExists then "exists" else "does not exist")
```

Цей код верифікує, чи існує директорія за вказаним шляхом, та виводить результат.

## Deep Dive (Поглиблений розбір)
Перевірки існування директорій суттєві з часів UNIX, де доступ до файлової системи мав стратегічне значення. У Haskell, ефективність і коректність роботи з файлами покладені на модуль `System.Directory`. Він надає функції `doesDirectoryExist` та `doesFileExist`. Альтернативи включають низькорівневе використання системних викликів (через `System.Posix` на POSIX-системах) або високорівневі бібліотеки, наприклад, `Shelly` чи `turtle` для більш складних сценаріїв.

Процес визначення наявності директорії з Haskell може відрізнятися відповідно до ОС, так як оперативні системи мають різні системні виклики та файлові системи. `System.Directory` абстрагує ці відмінності, пропонуючи портативний інтерфейс.

## See Also (Дивіться також)
- [Hackage – System.Directory](https://hackage.haskell.org/package/directory)