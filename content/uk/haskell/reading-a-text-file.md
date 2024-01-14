---
title:    "Haskell: Читання текстового файлу"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

Why:

Перерахування текстового файлу є важливим етапом в багатьох програмах, таких як обробка даних, інструменти пошуку і фільтрації, а також програми для аналізу тексту. Навчившись читати дані з текстового файлу у Haskell, ви зможете ефективно використовувати ці дані для своїх потреб.

## How To:

```Haskell
-- Читання текстового файлу
main = do
  -- Відкриття файлу для читання
  inh <- openFile "file.txt" ReadMode
  -- Прочитання всього файлу і збереження результату у змінну fileStr
  fileStr <- hGetContents inh
  -- Закриття файлу
  hClose inh
  -- Виведення вмісту файлу на екран
  putStrLn fileStr
```

Output:
```
This is a sample text file that we will read using Haskell.
З цим текстовим файлом ми будемо працювати у нашому прикладі.
```

## Deep Dive:

При читанні текстового файлу у Haskell, важливо пам'ятати про те, що дані будуть читатись починаючи з поточної позиції в файлі. Це означає, що після першого ж функціонального виклику, позиція зміщується на кінець прочитаного рядка. Також важливо зазначити, що при зчитуванні даних, вони автоматично перетворюються в рядок (String).

Для розбиття файла на окремі рядки, можна скористатись функцією ```lines```. Наприклад:

```Haskell
main = do
  inh <- openFile "file.txt" ReadMode
  -- Прочитання та розбиття файлу на окремі рядки
  fileLines <- fmap lines (hGetContents inh)
  hClose inh
  -- Виведення розбитих рядків на екран
  print fileLines
```

Output:
```
["This is a sample text file that we will read using Haskell.", "З цим текстовим файлом ми будемо працювати у нашому прикладі."]
```

## See Also:

- [Oficijnyj sajt Haskell](http://www.haskell.org/)
- [Komanda hGetContents u Haskell](https://hackage.haskell.org/package/base-4.11.1.0/docs/System-IO.html#v:hGetContents)