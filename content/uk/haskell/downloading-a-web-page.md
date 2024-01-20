---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що та чому?

Завантаження веб-сторінки - це процес отримання даних цієї сторінки з серверу. Програмісти це роблять, щоб обробити ці дані, витягти з них корисну інформацію або просто показувати сторінки користувачам.

## Як це зробити:

Перш ніж ми почнемо, встановіть бібліотеку `http-conduit` за допомогою команди `cabal install http-conduit`.

Тепер подивімось, як завантажити веб-сторінку в Haskell:

```Haskell
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

main = do
    simpleHttp "http://example.com" >>= L.writeFile "example.html"
```

За допомогою цього коду ми завантажуємо вміст веб-сторінки http://example.com і зберігаємо його у файлі `example.html`.

## Поглиблено:

Завантаження веб-сторінок було основний елемент інтернету з самого його початку. Проте методи його виконання значно вдосконалювалися. 

На початку було використовувано командні утиліти (як wget), але з появою високорівневих мов програмування виникли бібліотеки для завантаження веб-сторінок.

У Haskell є декілька бібліотек для роботи з HTTP, такі як `http-conduit`, `wreq`, `http-client` тощо, які мають свої особливості та переваги.

Використовуючи `http-conduit`, ми завантажуємо всі дані сторінки в пам'ять, що може бути неефективним для великих сторінок. Але для більшості випадків це працює досить добре.

## Дивіться також:

- [http-conduit on Hackage](http://hackage.haskell.org/package/http-conduit)
- [Haskell HTTP package comparison](https://wiki.haskell.org/Haskell_HTTP_package_comparison)
- [Haskell School of Music](http://www.haskellforall.com/2012/01/haskell-school-of-music.html)