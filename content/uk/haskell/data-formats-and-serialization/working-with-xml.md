---
date: 2024-01-26 04:32:25.925818-07:00
description: "\u042F\u043A: Haskell \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454\
  \ \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\
  \u0456 \u044F\u043A `xml-conduit`, \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\
  \u0438 \u0437 XML. \u041D\u0430\u0441\u0442\u0443\u043F\u043D\u0438\u0439 \u043F\
  \u0440\u0438\u043A\u043B\u0430\u0434 \u0434\u0435\u043C\u043E\u043D\u0441\u0442\u0440\
  \u0443\u0454 \u0430\u043D\u0430\u043B\u0456\u0437 \u0440\u044F\u0434\u043A\u0430\
  \ XML \u0456 \u0437\u0430\u043F\u0438\u0442 \u0434\u043E \u0435\u043B\u0435\u043C\
  \u0435\u043D\u0442\u0456\u0432."
lastmod: '2024-03-13T22:44:49.401014-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u0431\u0456\u0431\
  \u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\u0456 \u044F\u043A\
  \ `xml-conduit`, \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437\
  \ XML."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 XML"
weight: 40
---

## Як:
Haskell пропонує бібліотеки, такі як `xml-conduit`, для роботи з XML. Наступний приклад демонструє аналіз рядка XML і запит до елементів:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

main :: IO ()
main = do
  let xmlContent = "<greetings><hello>World!</hello></greetings>"
  let document = parseLBS_ def $ T.encodeUtf8 $ T.pack xmlContent
  let cursor = fromDocument document

  let helloTexts = cursor $// element "hello" &/ content
  print helloTexts  -- ['World!']
```

Приклад результату:

```
["World!"]
```

## Поглиблене вивчення
XML, який розшифровується як eXtensible Markup Language (розширювана мова розмітки), давно став стандартом серіалізації даних, на довго до популярності JSON. Він детальний, але жорсткий і стандартизований, що робить його підходящим для строгих корпоративних середовищ, застарілих систем та таких сфер, як фінанси та охорона здоров'я.

У Haskell існує кілька бібліотек для роботи з XML; однак, `xml-conduit` є однією з наймогутніших і широко використовуваних завдяки його ефективним можливостям стрімінгу та аналізу, що є частиною сімейства `conduit` для обробки потоків даних.

Альтернативи включають `HXT` (Haskell XML Toolbox), який використовує стрілки для аналізу та трансформації, пропонуючи інший парадигм для маніпуляцій з XML. Хоча `HXT` зараз менш популярний через його крутий кривою навчання, він все ще залишається міцним вибором для деяких випадків використання.

При реалізації обробки XML в Haskell, ви маєте піклуватися про кодування, оскільки рядки Haskell є Unicode, а дані XML можуть і не бути. Крім того, простори імен XML можуть додати додаткову складність до аналізу.

## Дивіться також:
- Документація пакету `xml-conduit`: https://hackage.haskell.org/package/xml-conduit
- Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- Книга "Real World Haskell", розділ 16, про обробку XML: http://book.realworldhaskell.org/read/xml.html
- Вікі Haskell про XML: https://wiki.haskell.org/XML
