---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:58:06.142235-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Пошук та заміна тексту — це засіб обробки рядків, що дозволяє знаходити фрагменти тексту та замінювати їх на інші. Програмісти роблять це для автоматизації редагування коду, швидкої модифікації даних чи масштабування змін.

## Як це зробити:
```Haskell
import Data.Text as T

-- Пошук та заміна простим текстом
replaceText :: String -> String -> String -> String
replaceText old new = T.unpack . T.replace (T.pack old) (T.pack new) . T.pack

-- Використання функції
main = putStrLn $ replaceText "cat" "dog" "The cat sat on the mat"

-- Результат:
-- "The dog sat on the mat"
```
Для регулярних виразів використовуємо `Text.Regex`:
```Haskell
import Text.Regex

-- Пошук та заміна за допомогою регулярних виразів
replaceRegex :: String -> String -> String -> String
replaceRegex regex new input =
  let compiledRegex = mkRegex regex
  in subRegex compiledRegex input new

-- Використання функції
main = putStrLn $ replaceRegex "c.t" "dog" "The cat sat on the mat"

-- Результат:
-- "The dog sat on the mat"
```

## Поглиблений розділ:
Пошук та заміна тексту бере свій початок ще від ранніх редакторів тексту, таких як `ed` і `sed` у Unix. Ці інструменти використовувались для роботи з текстовими файлами за допомогою командного рядка. У Haskell ми маємо ряд бібліотек, що надають функціональність для пошуку та заміни, включаючи `Data.Text` для простого тексту та `Text.Regex` при роботі з регулярними виразами.

Головна альтернатива — це використання стандартної бібліотеки `Prelude` з її функціями для роботи з рядками, але `Data.Text` пропонує покращену продуктивність з великими текстами.

Для регулярних виразів популярною альтернативою є `regex-pcre-builtin`, що використовує бібліотеку PCRE для мови Haskell, надаючи більш розширений синтаксис та контроль.

## Дивись також:
- [Документація для модуля Data.Text](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- [Документація для Text.Regex](https://hackage.haskell.org/package/regex-compat-0.95.1/docs/Text-Regex.html)
- [An introduction to regex in Haskell](https://wiki.haskell.org/Regular_expressions)
- [Інформація про регулярні вирази PCRE](http://www.pcre.org)
