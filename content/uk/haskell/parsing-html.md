---
title:                "Розбір html"
html_title:           "Haskell: Розбір html"
simple_title:         "Розбір html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Що і Чому?
Розбір HTML є процесом отримання структурованої інформації з HTML-коду. Програмісти зазвичай використовують розбір HTML для отримання необхідних даних з веб-сторінок, таких як ціни на товари, розклади подій або звіти про статистику.

## Як це зробити:
Розбір HTML можна зробити за допомогою багатьох бібліотек для Haskell, таких як "html-conduit", "html-tagsoup" або "htmlparser". Нижче подано приклад використання бібліотеки "html-tagsoup" для отримання всіх посилань на сторінці і їх відображення в консолі.

```Haskell
import Text.HTML.TagSoup -- імпорт бібліотеки

main :: IO ()
main = do
    -- завантаження HTML-сторінки з використанням "curl"
    html <- readProcess "curl" ["http://www.example.com"] []
    
    -- розбір HTML і отримання всіх тегів "a" (посилання) зі сторінки
    let links = filter (\(TagOpen _ attrs) -> "a" `elem` attrs) (parseTags html)
    
    -- виведення посилань в консоль
    mapM_ print ([fromAttrib "href" tag | tag <- links] :: [String]) 
```

Результат:

```
"http://www.example.com/page1"
"http://www.example.com/page2"
...
```

## Глибокий занурення:
Розбір HTML був популярним засобом отримання даних з веб-сторінок ще до появи API. Існує кілька альтернативних підходів до розбору HTML, таких як використання регулярних виразів або парсерів, які можна знайти в бібліотеках програмування мовою Python або Ruby.

У бібліотеці "html-tagsoup" для розбору HTML використовується подібна до XPath система запитів, яка дозволяє отримувати необхідні елементи зі сторінки з використанням коротких шаблонів.

## Дивитися також:
- [Hackage](https://hackage.haskell.org/package/html-conduit) - документація "html-conduit"
- [Official Haskell Website](https://www.haskell.org/) - офіційний сайт мови Haskell
- [Learn You a Haskell](http://learnyouahaskell.com/chapters) - безкоштовна онлайн-книга з навчання мові Haskell.