---
title:                "Tekst zoeken en vervangen"
date:                  2024-01-28T22:07:18.108639-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/haskell/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Tekst zoeken en vervangen stelt je in staat om strings te vinden en te wisselen. Programmeurs gebruiken het om code te updaten, te refactoren of snel gegevens te wijzigen.

## Hoe:

Laten we tekst zoeken en vervangen met Haskell. We gebruiken `Data.Text` voor Unicode tekstaanpak en efficiëntie. Zorg ervoor dat je `Data.Text` als volgt importeert:

```haskell
import qualified Data.Text as T
```

Nu, laten we alle instanties van "hello" naar "hi" vervangen in een tekst:

```haskell
replaceText :: T.Text -> T.Text -> T.Text -> T.Text
replaceText oud nieuw = T.replace oud nieuw

main :: IO ()
main = doen
  laat origineleTekst = T.pack "hello world, hello Haskell!"
  laat nieuweTekst = replaceText (T.pack "hello") (T.pack "hi") origineleTekst
  print nieuweTekst -- "hi world, hi Haskell!"
```

De `replace` functie doet het zware werk. We hebben het ingepakt in `replaceText` voor de duidelijkheid.

## Diepgaande Duik

Haskell's tekstvervangingsfuncties zoals `T.replace` zijn gebouwd op de arrayverwerkingsmogelijkheden van Haskell. Terugkijkend, Haskell werd voor het eerst bedacht in de jaren '80, met een focus op functioneel programmeren. Dit paradigma maakt operaties zoals tekstvervanging elegant en minder foutgevoelig vanwege onveranderlijkheid en sterke typesystemen.

Wat betreft alternatieven, je zou handmatig kunnen itereren over tekst en substrings vervangen, maar dat is meer foutgevoelig en inefficiënt.

De `Data.Text` bibliotheek gebruikt een andere interne voorstelling dan het `String` type (dat gewoon een lijst van karakters is), waardoor het beter geschikt is voor grootschalige tekstbewerkingen. De `T.replace` functie zelf maakt gebruik van efficiënte algoritmen voor het zoeken van strings, die zelfs voor grote teksten een goede prestatie bieden.

## Zie Ook

Voor meer over `Data.Text`, bekijk:

- [Text package op Hackage](https://hackage.haskell.org/package/text)

Overweeg ook breder te lezen over Haskell's stringmanipulatie:

- [Haskell Wiki over strings](https://wiki.haskell.org/Strings)
- [Learn You a Haskell for Great Good! over Tekst](http://learnyouahaskell.com/input-and-output#files-and-streams)
