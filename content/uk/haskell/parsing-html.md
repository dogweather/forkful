---
title:                "Haskell: Розбір html"
simple_title:         "Розбір html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Навіщо

Коли ви працюєте з інтернетом, ви часто стикаєтесь зі сторінками веб-сайтів, які містять багато структурованих даних. Наприклад, інформація про ціни, опис товарів, новини та багато іншого. Ці дані зберігаються у форматі HTML, який може бути складним для організації та аналізування. Тому, використовуючи інструменти для парсингу HTML у Haskell, ви зможете легко отримати та обробити ці дані для своїх потреб.

## Як

Для початку, вам потрібно отримати зміст сторінки HTML і зберегти його у змінній за допомогою функції `getResponseBody` з бібліотеки `http-conduit`. Наприклад:

```Haskell
import Network.HTTP.Conduit (simpleHttp)

main = do
    content <- simpleHttp "https://www.example.com"
    print content
```

Цей код доходить сторінку `"https://www.example.com"` та зберігає її вміст у змінній `content`. Далі, ви можете використати цей вміст та бібліотеку `tagsoup` для парсингу сторінки та отримання потрібних вам елементів. Наприклад, якщо вам потрібно отримати список заголовків з головної сторінки, ви можете використати такий код:

```Haskell
import Text.HTML.TagSoup (parseTags, Tag(TagOpen, TagClose))

getHeaders :: [Tag String] -> [String]
getHeaders [] = []
getHeaders (x:xs) =
    case x of
        TagOpen "h1" _ -> 
            let headerValue = takeWhile (/= '<') (head xs)
            in headerValue : getHeaders xs
        TagClose "h1" -> getHeaders xs
        _ -> getHeaders xs

main = do
    content <- simpleHttp "https://www.example.com"
    let tags = parseTags content
        headers = getHeaders tags
    print headers
```

У цьому коді ми використовуємо функцію `parseTags` для отримання списку тегів зі сторінки. Далі, ми проходимося цим списком, шукаючи теги `TagOpen` та `TagClose` з ім'ям `"h1"`, щоб отримати зміст заголовків. Зберігаючи їх у список `headers`, ми можемо роздрукувати його та отримати наступний результат:

```
["Перший заголовок", "Другий заголовок", "Третій заголовок"]
```

Ви можете використовувати більше різноманітних функцій та бібліотек для парсингу HTML у Haskell, щоб отримати потрібні вам дані зі сторінок веб-сайтів.

## Глибоке погруження

Парсинг HTML у Haskell може бути досить складним, оскільки HTML дуже гнучкий та не завжди коректний у своїй структурі