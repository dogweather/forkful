---
title:                "Haskell: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-html.md"
---

{{< edit_this_page >}}

# Hvorfor

I denne bloggposten skal vi se på hvordan du kan parsere HTML i Haskell. Parsere HTML er nyttig når du ønsker å ekstrahere spesifikk informasjon fra nettsider, for eksempel når du skal lage et nettstedsskrapeverktøy eller bygge en nettleser i Haskell.

# Hvordan Du Gjør Det

Parsere HTML i Haskell er ikke så vanskelig som du kanskje skulle tro. Takket være noen fantastiske biblioteker som finnes tilgjengelig, kan vi gjøre det med relativt få linjer med kode.

La oss si at vi ønsker å ekstrahere alle overskriftene i et HTML-dokument. Vi kan bruke biblioteket "html-conduit" for å gjøre dette. Her er et eksempel på hvordan vi kan gjøre det:

```Haskell
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor (Cursor, attributeIs, content, element, fromDocument, ($//))

findHeaders :: Cursor -> [String]
findHeaders = element "h1" & attributeIs "class" "header" & content

main :: IO ()
main = do
    html <- readFile "nettsted.html"
    let cursor = fromDocument $ parseLBS html
        headers = cursor $// findHeaders
    print headers
```

La oss gå gjennom koden: Først importerer vi funksjoner som lar oss jobbe med HTML-strukturen. Deretter definerer vi en funksjon som tar inn en "Cursor" (som representerer HTML-dokumentet), og returnerer en liste med overskriftene som finnes i dokumentet. I "main" -funksjonen leser vi inn HTML-dokumentet og konverterer det til en "Cursor", før vi bruker "findHeaders" -funksjonen for å finne alle overskriftene og skrive dem ut i konsollen.

Når vi kjører dette programmet, vil vi få følgende output:

```
["Dette er en overskrift 1", "Dette er en overskrift 2"]
```

Så enkelt er det å ekstrahere informasjon fra HTML-dokumenter i Haskell!

# Fordypning

For å virkelig forstå hvordan å parsere HTML i Haskell fungerer, er det viktig å forstå hvordan biblioteket "html-conduit" fungerer. Først og fremst, hva er en "Cursor"? En "Cursor" i dette tilfellet er en type som gir oss tilgang til XML-dokumentet, og gjør det enkelt å navigere rundt i det. Når vi bruker funksjoner som "&" eller "$//" i koden vår, er det egentlig "Cursor" som gjør det mulig for oss å bruke disse funksjonene.

En annen ting å merke seg er "element" og "attributeIs" funksjonene. Disse funksjonene lar oss finne spesifikke elementer og attributter i HTML-dokumentet basert på navn og verdi. Dette gjør det enkelt å finne akkurat den informasjonen vi er ute etter.

Vi kan også bruke lignende teknikker for å finne andre typer informasjon i HTML-dokumentet, som lenker, bilder eller tabeller. Det er bare å eksperimentere og finne ut hvilke funksjoner som passer best til dine behov.

# Se Også

- [html-conduit - Hackage](https://hackage.haskell.org/package/html-conduit)
- [html-conduit dokumentasjon](https://www.stackage.org/haddock/lts-8.13/html-conduit-1.2.3.2/Data-Conduit-HTML.html)
- [Eksempelkode for å parsere HTML i Haskell](https://github.com/sdiehl/haskell-parsing)