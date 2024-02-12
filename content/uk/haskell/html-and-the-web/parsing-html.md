---
title:                "Аналіз HTML"
aliases:
- /uk/haskell/parsing-html.md
date:                  2024-02-03T19:12:36.316105-07:00
model:                 gpt-4-0125-preview
simple_title:         "Аналіз HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Парсинг HTML в Haskell дозволяє вам видобувати дані, маніпулювати вмістом HTML або програмно взаємодіяти з веб-сторінками. Ця операція є незамінною для завдань, таких як веб-скрейпінг, автоматизоване тестування веб-додатків та майнінг даних з вебсайтів - використовуючи сильну систему типів Haskell і парадигми функціонального програмування для забезпечення надійного та стислого коду.

## Як:

Для парсингу HTML в Haskell ми використаємо бібліотеку `tagsoup` за її простоту та гнучкість. Спочатку, переконайтеся, що ви встановили бібліотеку, додавши `tagsoup` до файлу cabal вашого проєкту або виконавши команду `cabal install tagsoup`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- Приклад HTML для демонстрації
let sampleHtml = "<html><body><p>Вивчайте Haskell!</p><a href='http://example.com'>Натисніть тут</a></body></html>"

-- Парсинг HTML та фільтрація посилань (a теги)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- Друк витягнутих посилань
print links
```

Приклад виводу:
```plaintext
["http://example.com"]
```

Для більш складних потреб в парсингу HTML розгляньте використання бібліотеки `pandoc`, особливо якщо ви працюєте з конвертацією документів. Вона надзвичайно універсальна, але має більше складності:

```haskell
import Text.Pandoc

-- Припускаючи, що у вас завантажений документ Pandoc (doc), наприклад, з читання файлу
let doc = ... -- Тут розміщується ваш документ Pandoc

-- Конвертування документу в рядок HTML
let htmlString = writeHtmlString def doc

-- Тепер, ви могли б парсити `htmlString` як описано вище або продовжувати відповідно до ваших потреб.
```
Майте на увазі, що `pandoc` - це набагато більша бібліотека, яка фокусується на конвертації між численними форматами розмітки, тому використовуйте її, якщо вам потрібні ці додаткові можливості або якщо ви вже працюєте з форматами документів у вашому додатку.
