---
title:                "Пошук та заміна тексту"
html_title:           "C++: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Пошук і заміна тексту - це процес знаходження конкретних наборів символів (рядків) та їхньої заміни. Програмісти роблять це, щоб автоматизувати та спростити редагування коду.

## Як це зробити:

Ось приклад використання стандартного модуля `Data.Text` в Haskell для пошуку та заміни тексту:

```Haskell
import qualified Data.Text as T

searchAndReplace :: T.Text -> T.Text -> T.Text -> T.Text
searchAndReplace old new = T.replace old new

ghci> searchAndReplace "Hello" "Hi" "Hello, World!"
"Hi, World!"
```
В цьому прикладі функція `searchAndReplace` шукає рядок "Hello" та замінює його на "Hi".

## Поглиблений матеріал

1. Історичний контекст: Функції для пошуку та заміни тексту існують у багатьох мовах програмування, включаючи JavaScript, Python, та інші. Вони утворюють основу для редагування тексту у багатьох додатках та інструментах.
   
2. Альтернативи: Хоча `Data.Text` надає прямолінійний спосіб пошуку та заміни, інші можливості включають використання регулярних виразів через модуль `Text.Regex`, який набагато більш потужний, але й складніший.
   
3. Деталі реалізації: `T.replace` працює шляхом розбивання вхідного тексту на фрагменти, збігається з пошуковим текстом, а потім об'єднує їх назад, вставляючи новий текст замість старого.

## Дивіться також:

- [Data.Text documentation](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
- [An introduction to regex in Haskell](https://wiki.haskell.org/Regex)