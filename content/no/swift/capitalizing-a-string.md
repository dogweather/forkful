---
title:                "Swift: Store bokstaver i en streng"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne formatere strenger riktig er en viktig del av Swift-programmering. Det tillater oss å gjøre teksten mer leselig og tilgjengelig for brukeren. Kapitalisering av en streng kan være nyttig når du for eksempel vil fremheve viktige ord eller gi overskrifter til deler av teksten din. I denne bloggposten vil vi lære deg hvordan du kan kapitalisere en streng i Swift.

## Hvordan gjøre det

For å kapitalisere en streng i Swift, kan du bruke funksjonen `capitalized` som er tilgjengelig på alle `String`-objekter. La oss ta en titt på et eksempel:

```Swift
let tekst = "dette er en tekst"
let kapitalisertTekst = tekst.capitalized\\
```

Denne kodesnutten vil gi oss følgende output:

```
"Dette er en tekst"
```

Vi kan også bruke `capitalized`-funksjonen på et `String`-objekt som allerede er delvis kapitalisert:

```Swift
let tekst = "Dette ER en tekst"
let kapitalisertTekst = tekst.capitalized
```

Output:

```
"Dette er en tekst"
```

Som du kan se, vil funksjonen automatisk konvertere alle bokstaver til små bokstaver og deretter kun kapitalisere den første bokstaven i hvert ord.

## Dypdykk

For å få en dypere forståelse av hvordan `capitalized`-funksjonen fungerer, kan du ta en titt på følgende eksempel:

```Swift
let tekst = "DENNE TEKSTEN SKAL ALLE ORD VÆRE I STORE BOKSTAVER"
let kapitalisertTekst = tekst.capitalized
```

Output:

```
"Denne Teksten Skal Alle Ord Være I Store Bokstaver"
```

Som du ser, vil funksjonen også ta hensyn til ord som allerede er skrevet i store bokstaver. Den vil bare kapitalisere den første bokstaven i hvert ord og beholde resten av bokstavene som de er.

## Se også

* [Swift Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
* [String Methods](https://www.hackingwithswift.com/example-code/strings/how-to-capitalize-the-first-letter-of-a-string)
* [Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)

Vi håper denne bloggposten har gitt deg en bedre forståelse av hvordan du kan kapitalisere strenger i Swift. For mer informasjon om strenger og deres metoder, anbefaler vi å sjekke ut de nevnte ressursene. Lykke til med din Swift-programmering!