---
title:                "Kotlin: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Du har kanskje kommet over situasjoner der du trenger å fjerne bestemte tegn i en tekststreng. Dette kan være for å rengjøre data eller for å lage et mer leselig resultat. Uansett grunn, så er det viktig å forstå hvordan man kan slette tegn som matcher et bestemt mønster i Kotlin.

## Hvordan

For å slette tegn som matcher et mønster i Kotlin, kan du bruke funksjonen `Regex.replace()` og gi den to argumenter - tekststrengen du ønsker å modifisere, og et mønster som beskriver hvilke tegn som skal slettes.

```Kotlin
val tekst = "Hei! Jeg elsker å kode i Kotlin!"
val nyTekst = Regex("[! ]").replace(tekst, "")
```

Her vil `nyTekst` variabelen inneholde strengen "HeiJegelskeråkodeiKotlin" siden funksjonen slettet alle utropstegn og mellomrom. Dette kan være nyttig hvis du for eksempel ønsker å fjerne spesielle tegn fra en brukers input.

## Dypere dykk

Det er verdt å merke seg at `Regex.replace()` funksjonen returnerer en ny tekststreng og ikke endrer den originale strengen. Så hvis du vil at endringene skal skje på selve variabelen, må du tildele den nye verdien til variabelen.

I tillegg kan du også bruke regex grupper i mønsteret for å slette spesifikke deler av teksten. For eksempel, hvis du ønsker å fjerne alle tall fra en tekststreng, kan du bruke mønsteret `[0-9]` som beskriver et tall.

## Se også

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Regex Class in Kotlin]( https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)