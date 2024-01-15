---
title:                "Å finne lengden av en streng"
html_title:           "Haskell: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden til en streng er en vanlig oppgave i mange programmeringsspråk, inkludert Haskell. Det er nyttig å kunne gjøre dette for å kunne behandle og manipulere strenger på en effektiv måte.

## Hvordan
For å finne lengden til en streng i Haskell, kan vi bruke den innebygde funksjonen `length`. Denne funksjonen tar inn en streng som argument og returnerer antall tegn i strengen. La oss se på et eksempel:

```Haskell
length "Hei, verden!"

-- output: 12
```

Vi kan også bruke denne funksjonen på variabler som inneholder strenger. La oss si at vi har en variabel `navn` som inneholder navnet vårt. Da kan vi finne lengden til denne strengen ved å skrive:

```Haskell
length navn

-- output: 5 (hvis navnet er "Hans")
```

Vi kan også bruke `length` på symboliske strenger, som for eksempel tall. La oss si at vi vil finne antall sifre i tallet 123456. Da kan vi skrive:

```Haskell
length (show 123456)

-- output: 6
```

## Dykk dypere

Hvis du vil forstå hvordan `length` funksjonen egentlig fungerer, kan du dykke dypere inn i Haskell sine strengmanipuleringsfunksjoner. En streng i Haskell er egentlig bare en liste med tegn. Dette betyr at lengden til en streng egentlig er lengden til denne listen. Derfor fungerer `length` ved å telle antall elementer i listen som representerer strengen.

## Se også

- [Haskell dokumentasjon for `length`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-List.html#v:length)
- [Haskell lærebøker på norsk](https://wiki.haskell.org/Norsk)
- [Enkle intro til Haskell på norsk](https://martinlearnsruby.github.io/examples/haskell-sanity-check-0.6.3.pdf)