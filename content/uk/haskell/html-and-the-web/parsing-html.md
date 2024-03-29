---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:36.316105-07:00
description: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u0432 Haskell \u0434\
  \u043E\u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0430\u043C \u0432\u0438\u0434\
  \u043E\u0431\u0443\u0432\u0430\u0442\u0438 \u0434\u0430\u043D\u0456, \u043C\u0430\
  \u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u0442\u0438 \u0432\u043C\u0456\u0441\
  \u0442\u043E\u043C HTML \u0430\u0431\u043E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u043D\u043E \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u044F\u0442\u0438\
  \ \u0437 \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0430\u043C\
  \u0438. \u0426\u044F \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u044F \u0454 \u043D\
  \u0435\u0437\u0430\u043C\u0456\u043D\u043D\u043E\u044E \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:49.354458-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML \u0432 Haskell \u0434\u043E\
  \u0437\u0432\u043E\u043B\u044F\u0454 \u0432\u0430\u043C \u0432\u0438\u0434\u043E\
  \u0431\u0443\u0432\u0430\u0442\u0438 \u0434\u0430\u043D\u0456, \u043C\u0430\u043D\
  \u0456\u043F\u0443\u043B\u044E\u0432\u0430\u0442\u0438 \u0432\u043C\u0456\u0441\u0442\
  \u043E\u043C HTML \u0430\u0431\u043E \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043D\
  \u043E \u0432\u0437\u0430\u0454\u043C\u043E\u0434\u0456\u044F\u0442\u0438 \u0437\
  \ \u0432\u0435\u0431-\u0441\u0442\u043E\u0440\u0456\u043D\u043A\u0430\u043C\u0438\
  . \u0426\u044F \u043E\u043F\u0435\u0440\u0430\u0446\u0456\u044F \u0454 \u043D\u0435\
  \u0437\u0430\u043C\u0456\u043D\u043D\u043E\u044E \u0434\u043B\u044F\u2026"
title: "\u0410\u043D\u0430\u043B\u0456\u0437 HTML"
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
