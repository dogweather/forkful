---
title:                "Å finne lengden på en streng"
html_title:           "Haskell: Å finne lengden på en streng"
simple_title:         "Å finne lengden på en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lengden til en streng i programmering er antallet tegn eller bokstaver som finnes i en streng. Det kan være nyttig å vite lengden til en streng når man skal manipulere eller behandle data. 

## Hvordan:
For å finne lengden til en streng i Haskell, kan vi bruke den innebygde funksjonen `length`. Denne funksjonen tar inn en liste av elementer, inkludert strenger, og returnerer lengden som en numerisk verdi. Her er et eksempel på hvordan du kan bruke den:

```Haskell
length "Hei verden!" 
```
Dette vil returnere verdien 11, siden det er 11 tegn i strengen "Hei verden!". Du kan også bruke `length` på mer komplekse strukturer som lister:

```Haskell
length ["apple", "banana", "orange"] 
```
Dette vil returnere verdien 3, siden det er tre elementer i listen.

## Dykk dypere:
I eldre versjoner av Haskell måtte man lage en egen funksjon for å finne lengden til en streng. Dette førte ofte til at man måtte bruke løkker og telle variabler for å finne lengden, noe som kunne være tidkrevende og komplisert. Med den innebygde `length`-funksjonen i dagens versjoner av Haskell, blir dette mye enklere og mer effektivt.

En alternativ måte å finne lengden til en streng i Haskell på er å bruke funksjonen `Data.Text.length` fra pakken `text`. Denne funksjonen kan være nyttig når man arbeider med store tekstmengder på grunn av sin høye ytelse. 

Når man ser på implementasjonen av `length`-funksjonen i Haskell, vil man se at den bruker prinsippet om rekursjon for å telle elementene i listen. Dette er en effektiv måte å finne lengden på, spesielt for store lister.

## Se også:
- [Lengdefunksjonen i Haskell dokumentasjonen](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:length)
- [Alternativ til `length`-funksjonen: `Data.Text.length` fra `text`-pakken](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html#v:length)