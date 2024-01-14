---
title:    "Haskell: Перевірка наявності каталогу"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

Uкраїнський блог про програмування на Haskell: 

## Чому 

Перевірка наявності каталогу варта уваги, оскільки це може бути важливим кроком у вирішенні конкретної задачі або запобіганні можливим проблемам.

## Як це зробити

Перевірка наявності каталогу можлива за допомогою функції `doesDirectoryExist` з модуля `System.Directory`. Перед тим, як викликати цю функцію, потрібно імпортувати модуль за допомогою `import System.Directory`.

```Haskell
import System.Directory

main = do
  let directory = "C:\\Users\\User\\Documents\\"
  exists <- doesDirectoryExist directory
  print exists
```

У цьому прикладі ми перевіряємо наявність каталогу "Documents" на диску C. Якщо каталог існує, виводиться `True`, якщо ні - `False`.

## Глибока занурення

Для отримання більш детальної інформації про перевірку наявності каталогу, можна обрати сигнатуру функції `doesDirectoryExist` за допомогою `:t` у ghci. 

```Haskell
:t doesDirectoryExist 
```

Отримаємо вивід:

```
doesDirectoryExist :: FilePath -> IO Bool
```

Пояснення: `FilePath` - це тип даних, який представляє шлях до файлу або каталогу. `IO` означає, що виклик цієї функції буде виконувати дію вводу-виводу. `Bool` вказує на те, що функція повертає булеве значення.

## Дивіться також

- [Робота з файловою системою в Haskell](https://www.haskell.org/cabal/users-guide/developing-packages.html#file-system-operations)
- [Документація з функції `doesDirectoryExist`](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)