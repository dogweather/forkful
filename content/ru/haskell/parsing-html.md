---
title:                "Разбор HTML"
date:                  2024-01-29T00:00:06.714647-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Разбор HTML означает извлечение данных из HTML-документов — в конце концов, HTML - это каркас веба. Программисты анализируют HTML, чтобы автоматизировать сбор данных, перенести содержимое или преобразовать его в различные форматы.

## Как:

Давайте погрузимся в код, используя библиотеку `tagsoup` для разбора простого HTML-фрагмента. Сначала убедитесь, что установили пакет из Hackage через `cabal install tagsoup`.

```Haskell
import Text.HTML.TagSoup

-- Давайте разберем простой HTML-фрагмент
let html = "<html><body><p>Привет, Haskell!</p></body></html>"

-- Разбираем
let parsedHtml = parseTags html

-- Находим параграфы
let paragraphs = partitions (~== "<p>") parsedHtml

-- Получаем текст из первого параграфа
let firstParagraphText = innerText $ head paragraphs

-- Вуаля!
print firstParagraphText
```

Пример вывода:
```
"Привет, Haskell!"
```
Этот фрагмент разбирает строку HTML, отыскивает теги параграфов и печатает текст, содержащийся в первом параграфе. Опрятно и сладко.

## Глубокое Погружение

Разбор HTML в Haskell не всегда был таким простым, как сегодня. Когда-то люди создавали свои собственные парсеры или боролись с библиотеками низкого уровня, анализируя HTML, как будто это был Дикий Запад.

В наши дни у вас есть выбор. `tagsoup`, как мы использовали, отлично подходит, когда структура HTML скорее предложение, чем правило — он терпим к реальному неаккуратному HTML. Если вы ищете большей строгости, `html-conduit` в сочетании с `xml-conduit` из пакета `conduit` может быть вашим выбором. Они используют потоковый подход и более требовательны к структуре.

Под капотом эти библиотеки превращают HTML в дерево или суп из тегов. Они предоставляют удобные функции для запроса и манипулирования этими данными, делая разбор HTML менее головной болью. Думайте о них как о карте сокровищ, где X отмечает тег параграфа.

## См. Также

- [`tagsoup` на Hackage](https://hackage.haskell.org/package/tagsoup)
- [`html-conduit` на Hackage](https://hackage.haskell.org/package/html-conduit)
- [Документация Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/) - Хотя и не Haskell, подход Beautiful Soup к 'супу из тегов' повлиял на похожие библиотеки в мире Haskell.
- [Функции и операторы XPath и XQuery на сайте W3C](https://www.w3.org/TR/xpath-functions/) - Глубокое погружение в стандарты может информировать о структуре и запросах к XML/HTML документам, полезно для понимания стратегий разбора на заднем плане.
