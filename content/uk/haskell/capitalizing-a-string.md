---
title:                "Капіталізація рядка"
html_title:           "Haskell: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Що & Чому?
Капіталізація рядка - це процес перетворення першої літери кожного слова у рядку на велику літеру. Програмісти часто використовують цю техніку для полегшення читабельності та надання більш професійного вигляду своїм текстам.

Як?
```Haskell
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs
```
Вхід: "hello world"
Вихід: "Hello world"

```Haskell
capitalizeWords :: String -> String
capitalizeWords "" = ""
capitalizeWords xs = unwords (map (\x -> toUpper (head x) : tail x) (words xs))
```
Вхід: "hello world"
Вихід: "Hello World"

Глибока занурення
Історичний контекст: Техніка капіталізації рядків походить з типографіки, де це використовувалося для виділення заголовків та назв у текстах. У програмуванні, це стало популярним для підвищення читабельності та надання більш структурованого вигляду коду.

Альтернативи: Існує багато варіантів реалізації капіталізації рядків в Haskell, такі як використання функцій `map` та `interact` чи використання комплексних бібліотек.

Деталі реалізації: У приведених прикладах використовується вбудована функція `toUpper` для перетворення символу велику літеру. Метод `capitalize` перетворює першу літеру рядка на велику, тоді як метод `capitalizeWords` перетворює першу літеру кожного слова на велику.

Див. також
- [String manipulation](https://wiki.haskell.org/String_manipulation) на сторінці Haskell Wiki
- [Capitalization](https://en.wikipedia.org/wiki/Capitalization) на вікіпедії.