---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-html.md"
---

{{< edit_this_page >}}

### Що та чому?
Аналіз HTML - це процес розбиття коду HTML на його складові для подальшої обробки в програмі. Це допомагає програмістам витягувати, маніпулювати та використовувати дані з HTML-документів.

### Як зробити:
Ми можемо використовувати бібліотеку `tagsoup` в Haskell для аналізу HTML. Подивимося на невеликий приклад:
```Haskell
import Text.HTML.TagSoup

main :: IO ()
main = do
    let html = "<html><body><p>Hello, World!</p></body></html>"
    let tags = parseTags html
    print tags
```
Виведе:
```Haskell
[TagOpen "html" [],TagOpen "body" [],TagOpen "p" [],TagText "Hello, world!",TagClose "p",TagClose "body",TagClose "html"]
```
Вище ми подали строку HTML до функції `parseTags` та вивели результати.

### Поглиблений розгляд:
Основним способом аналізу HTML в Haskell є бібліотека `tagsoup`. Вона бере неправильний HTML та повертає список тегів, які можна легко проаналізувати. Слід пам'ятати, що є й інші бібліотеки для аналізу HTML, такі як `html-conduit` та `hxt`, але `tagsoup` дає більше гнучкості при аналізі невідповідного HTML.

### Дивіться також:
1. Офіційна документація: http://hackage.haskell.org/package/tagsoup
2. Інші методи аналізу HTML в Haskell: http://www.yesodweb.com/book/xml
3. Гайд по Tagsoup (англійською): https://www.fpcomplete.com/blog/2015/04/scraping-html-using-tagsoup