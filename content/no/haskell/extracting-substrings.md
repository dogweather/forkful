---
title:                "Haskell: Uttrekking av understrenger"
simple_title:         "Uttrekking av understrenger"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å utvinne substrings, eller delmønstre, fra en streng er en viktig oppgave i dataprogrammering. Det lar oss manipulere og behandle tekst på en mer effektiv og presis måte.

## Hvordan

Det finnes flere forskjellige måter å extrahere substrings i Haskell. En av de mest brukte er funksjonen `take` som lar oss hente ut et bestemt antall tegn fra en streng. For eksempel, hvis vi har en streng `tekst = "Hei, dette er en streng"`, kan vi bruke følgende kode for å hente de første 5 tegnene:

```
tekst = "Hei, dette er en streng"
take 5 tekst
```

Dette vil gi oss følgende output:

```
Hei, 
```

En annen måte å extrahere substrings på er ved å bruke funksjonen `drop`, som lar oss fjerne et bestemt antall tegn fra starten av en streng. For eksempel, hvis vi ønsker å fjerne de første 4 tegnene fra `tekst`-strengen, kan vi bruke følgende kode:

```
drop 4 tekst
```

Dette vil gi oss følgende output:

```
 dette er en streng
```

Vi kan også bruke funksjonen `substring` for å hente ut en del av en streng basert på start- og sluttposisjon. For eksempel, hvis vi ønsker å hente ut ordet "dette" fra `tekst`-strengen, kan vi bruke følgende kode:

```
substring 5 9 tekst
```

Dette vil gi oss følgende output:

```
dette
```

## Dype dypdykk

I tillegg til funksjonene nevnt ovenfor, finnes det flere andre måter å extrahere substrings i Haskell på. For eksempel, kan vi bruke mønstermatching og listekomprehensjon for å hente ut deler av en streng basert på mønstre eller betingelser. Det finnes også spesielle biblioteker som for eksempel `Text.Regex` som lar oss bruke regulære uttrykk for å finne og manipulere deler av en streng.

## Se også

- [Haskell hovedside](https://www.haskell.org/)
- [Haskell wiki](https://wiki.haskell.org/)
- [Haskell dokumentasjon](https://hackage.haskell.org/)