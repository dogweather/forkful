---
title:                "Зробити першу літеру рядка великою"
aliases: - /uk/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:41.180552-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Велика літера в рядку передбачає перетворення першої літери даного рядка у верхній регістр, одночасно забезпечуючи, щоб решта літер залишились у нижньому регістрі. Програмісти роблять це для форматування виводів, дотримання граматичної вірності у текстах або підвищення читабельності генерованих даних.

## Як це зробити:
В Haskell ви можете зробити першу літеру рядка великою, використовуючи стандартну бібліотеку, без необхідності використання сторонніх бібліотек.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Приклад використання:
main = putStrLn $ capitalize "hello world"
```

Вивід:
```
Hello world
```

Для більш складних сценаріїв або для зручності можна використовувати сторонню бібліотеку, таку як `text`, яка є популярною для ефективної маніпуляції з рядками в Haskell.

Спочатку, вам потрібно додати `text` до залежностей вашого проекту. Потім ви можете використовувати його функції для того, щоб зробити першу літеру рядка великою таким чином:

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Приклад використання з бібліотекою text:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Вивід:
```
Hello world
```

Обидва ці приклади демонструють прості, та ефективні способи зробити першу літеру рядка великою в Haskell, з використанням або без використання сторонніх бібліотек.
