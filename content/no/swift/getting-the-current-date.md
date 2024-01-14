---
title:    "Swift: Å få nåværende dato"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne få tak i den nåværende datoen er en grunnleggende funksjon i mange programmeringsprosjekter. Enten det handler om å vise den aktuelle datoen på en nettside, lagre den i en database eller bare for å holde oversikt over tid, så er evnen til å få tak i den nåværende datoen essensiell. I denne bloggposten vil vi vise deg hvordan du enkelt kan få tak i den nåværende datoen ved hjelp av Swift.

## Hvordan gjøre det

For å kunne få tak i den nåværende datoen i Swift, kan du bruke klassen `Date`. Dette er en innebygd klasse i Swift som lar deg håndtere datoer og tider. For å få tak i den nåværende datoen, kan du bruke funksjonen `Date()`, som vil returnere et `Date` objekt med den nåværende datoen og tiden.

La oss se på et eksempel på hvordan dette kan implementeres i praksis:

```Swift
let nåværendeDato = Date()
print(nåværendeDato) // output: 2019-12-24 15:00:00 +0000
```

Som du kan se, vil utskriften gi deg den nåværende datoen og tiden i GMT-format. Hvis du ønsker å konvertere datoen til din lokale tidssone, kan du bruke klassen `DateFormatter`. La oss se på hvordan du kan gjøre det:

```Swift
let nåværendeDato = Date()
let formatter = DateFormatter()
formatter.timeZone = TimeZone.current
formatter.dateFormat = "dd.MM.yyyy"
let lokalDato = formatter.string(from: nåværendeDato)
print(lokalDato) // output: 24.12.2019
```

Her oppretter vi en instans av `DateFormatter` og setter tidssonen til den lokale tidssonen. Deretter definerer vi et format for datoen, i dette tilfellet dag.måned.år. Til slutt konverterer vi datoen og skriver ut den resulterende strengen med den nåværende datoen i lokal tid.

## Dypdykk

I tillegg til å få tak i den nåværende datoen, kan du også bruke `Date` klassen til å utføre mer avanserte operasjoner, som for eksempel å beregne forskjellen mellom to datoer. Du kan også bruke metoder som `addingTimeInterval` for å legge til eller trekke fra en viss tid fra en dato. For en mer grundig forståelse av `Date` klassen og dens funksjoner, kan du sjekke dokumentasjonen fra Apple.

## Se også

- [Apple dokumentasjon om Date klassen](https://developer.apple.com/documentation/foundation/date)
- [Pragmatic Programmers' guide til dato og tidshåndtering i Swift](https://pragprog.com/book/mrswift/learn-apple-s-swift-language-for-ios)