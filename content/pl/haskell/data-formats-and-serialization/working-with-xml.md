---
date: 2024-01-26 04:31:56.683093-07:00
description: "Jak to zrobi\u0107: Haskell oferuje takie biblioteki jak `xml-conduit`\
  \ do obs\u0142ugi XML. Poni\u017Cszy przyk\u0142ad demonstruje parsowanie ci\u0105\
  gu XML i kwerendowanie\u2026"
lastmod: '2024-03-13T22:44:35.478236-06:00'
model: gpt-4-0125-preview
summary: "Haskell oferuje takie biblioteki jak `xml-conduit` do obs\u0142ugi XML."
title: Praca z XML
weight: 40
---

## Jak to zrobić:
Haskell oferuje takie biblioteki jak `xml-conduit` do obsługi XML. Poniższy przykład demonstruje parsowanie ciągu XML i kwerendowanie elementów:

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

Przykładowe wyjście:

```
["World!"]
```

## Szczegółowa analiza
XML, czyli eXtensible Markup Language, jest podstawą serializacji danych na długo przed wzrostem popularności JSON. Jest rozwlekły, ale sztywny i ustandaryzowany, co sprawia, że nadaje się do ścisłych środowisk korporacyjnych, systemów przestarzałych oraz branż takich jak finanse i opieka zdrowotna.

Haskell posiada kilka bibliotek do XML; jednak `xml-conduit` należy do najmocniejszych i najczęściej używanych ze względu na jego efektywne możliwości przetwarzania strumieniowego i parsowania, będąc częścią rodziny `conduit` do obsługi strumieni danych.

Do alternatyw należy `HXT` (Haskell XML Toolbox), który do parsowania i transformacji używa strzałek (arrows), oferując inny paradygmat do manipulacji XML. Mimo że `HXT` jest obecnie mniej popularny z powodu wyższej krzywej uczenia się, nadal pozostaje solidnym wyborem dla niektórych przypadków użycia.

Implementując przetwarzanie XML w Haskellu, musisz zwrócić uwagę na kodowanie, ponieważ ciągi znaków w Haskellu są Unicode, a dane XML mogą nim nie być. Ponadto, przestrzenie nazw XML mogą dodać dodatkową złożoność do parsowania.

## Zobacz również:
- Dokumentacja pakietu `xml-conduit`: https://hackage.haskell.org/package/xml-conduit
- Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- Książka "Real World Haskell", Rozdział 16, o obsłudze XML: http://book.realworldhaskell.org/read/xml.html
- Wiki Haskell o XML: https://wiki.haskell.org/XML
