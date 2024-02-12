---
title:                "Werken met XML"
aliases:
- /nl/haskell/working-with-xml.md
date:                  2024-01-28T22:11:41.562649-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met XML in Haskell omvat het parsen, manipuleren en genereren van XML-structuren. Programmeurs gaan om met XML om te interacteren met tal van applicaties en protocollen die XML als hun dataformaat gebruiken, zoals webdiensten en configuratiebestanden.

## Hoe:

Haskell biedt bibliotheken zoals `xml-conduit` voor het omgaan met XML. Het volgende voorbeeld demonstreert het parsen van een XML-string en het opvragen van elementen:

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

Voorbeelduitvoer:

```
["World!"]
```

## Diepere Duik

XML, kort voor eXtensible Markup Language, is al lang voor de opkomst van JSON een standaard geweest in data-serialisatie. Het is langdradig, maar strikt en gestandaardiseerd, waardoor het geschikt is voor strikte zakelijke omgevingen, legacy-systemen en industrieën zoals financiën en gezondheidszorg.

Haskell heeft meerdere bibliotheken voor XML; echter, `xml-conduit` is een van de krachtigste en meest gebruikte vanwege zijn efficiënte streaming- en parseermogelijkheden, onderdeel van de `conduit`-familie voor het omgaan met datastromen.

Alternatieven zijn onder meer `HXT` (Haskell XML Toolbox) dat pijlen gebruikt voor het parsen en transformeren, en biedt een ander paradigma voor XML-manipulaties. Hoewel `HXT` nu minder populair is vanwege de steilere leercurve, blijft het nog steeds een solide keuze voor sommige gebruikssituaties.

Bij het implementeren van XML-verwerking in Haskell, moet je rekening houden met codering, aangezien Haskell-strings Unicode zijn en XML-data dat misschien niet is. Bovendien kunnen XML-naamruimten extra complexiteit toevoegen aan het parsen.

## Zie Ook:

- De documentatie van het pakket `xml-conduit`: https://hackage.haskell.org/package/xml-conduit
- De Haskell XML Toolbox (HXT): http://hackage.haskell.org/package/hxt
- Het boek "Real World Haskell", Hoofdstuk 16, voor XML-behandeling: http://book.realworldhaskell.org/read/xml.html
- Haskell Wiki over XML: https://wiki.haskell.org/XML
