---
date: 2024-01-26 04:31:53.537139-07:00
description: "Hur man g\xF6r: Haskell erbjuder bibliotek som `xml-conduit` f\xF6r\
  \ att hantera XML. F\xF6ljande exempel demonstrerar tolkning av en XML-str\xE4ng\
  \ och s\xF6kning efter\u2026"
lastmod: '2024-03-13T22:44:37.978993-06:00'
model: gpt-4-0125-preview
summary: "Haskell erbjuder bibliotek som `xml-conduit` f\xF6r att hantera XML."
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
Haskell erbjuder bibliotek som `xml-conduit` för att hantera XML. Följande exempel demonstrerar tolkning av en XML-sträng och sökning efter element:

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

Exempel på utskrift:

```
["World!"]
```

## Fördjupning
XML, förkortning för eXtensible Markup Language, har varit en grundpelare i data-serialisering långt innan JSON:s framfart. Det är utförligt, men strängt och standardiserat, vilket gör det lämpligt för strikta företagsmiljöer, äldre system och industrier som finans och sjukvård.

Haskell har flera bibliotek för XML; dock är `xml-conduit` bland de mest kraftfulla och mest använda tack vare dess effektiva streaming- och tolkningsförmåga, som en del av `conduit`-familjen för hantering av dataströmmar.

Alternativ inkluderar `HXT` (Haskell XML Toolbox) som använder pilar för tolkning och transformation, vilket erbjuder ett annat paradigm för XML-manipulationer. Även om `HXT` är mindre populärt nu på grund av dess brantare inlärningskurva, kvarstår det fortfarande som ett solitt val för vissa användningsfall.

När du implementerar XML-bearbetning i Haskell, måste du vara uppmärksam på kodning, eftersom Haskell-strängar är Unicode och XML-data kanske inte är det. Dessutom kan XML-namnrymder lägga till extra komplexitet i tolkningen.

## Se även:
- Dokumentationen för `xml-conduit`-paketet: https://hackage.haskell.org/package/xml-conduit
- Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- Boken "Real World Haskell", Kapitel 16, för hantering av XML: http://book.realworldhaskell.org/read/xml.html
- Haskell Wiki om XML: https://wiki.haskell.org/XML
