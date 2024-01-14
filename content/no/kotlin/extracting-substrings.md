---
title:    "Kotlin: Ekstrahering av delstrenger"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor

Når vi jobber med strenger i Kotlin, kan det ofte være nyttig å kunne hente ut deler av en streng, også kalt en substrings. Dette kan være nyttig når vi for eksempel trenger å behandle data eller informasjon som er lagret i en bestemt format, og som vi ønsker å hente ut enkeltdeler av.

## Slik gjør du det

Vi kan enkelt ekstrahere substrings ved å bruke en innebygd funksjon i Kotlin kalt `substring()`. Denne funksjonen tar to parametere: startindeksen og sluttindeksen for den delen av strengen vi ønsker å hente ut. La oss se på et eksempel:

```Kotlin
val navn = "Ole"
val fletterennavn = navn.substring(0, 1) // henter ut første bokstav
println(fletterennavn) // vil skrive ut "O"
```

Vi kan også bruke negative tall som indekser, hvor `-1` tilsvarer den siste bokstaven i strengen og `-2` betyr den nest siste bokstaven, og så videre. Her er et eksempel på hvordan vi kan bruke dette i praksis:

```Kotlin
val setning = "Hei, mitt navn er Lise"
val mellomnavn = setning.substring(15, -6) // henter ut mellomnavn
println(mellomnavn) // vil skrive ut "Lise"
```

Vi kan også bruke en variant av `substring()` som bare tar en startindeks som parameter, i tilfelle vi ønsker å hente ut en del av strengen fra og med startindeksen og helt til slutten av strengen. Her er et eksempel på hvordan vi kan bruke dette:

```Kotlin
val setning = "Vi elsker Kotlin"
val elsker = setning.substring(3) // henter ut alt fra og med indeks 3
println(elsker) // vil skrive ut "elsker Kotlin"
```

## Dykk dypere

Når vi bruker `substring()` i Kotlin, må vi være oppmerksomme på at denne funksjonen tar med seg mellomrom og spesialtegn som en del av substringen. Dette kan føre til uventet oppførsel hvis vi ikke har dette i tankene når vi lager programmene våre. Det kan derfor være lurt å kombinere `substring()` med en annen funksjon kalt `trim()` for å fjerne eventuelle mellomrom eller spesialtegn før vi lagrer substringen i en variabel.

Vi kan også bruke `substring()` til å hente ut deler av en streng basert på et mønster ved hjelp av regex-uttrykk. Dette kan være nyttig når vi for eksempel ønsker å hente ut tall eller bokstaver fra en streng. Det finnes mange ressurser på nettet som kan hjelpe oss med å lære mer om regex og hvordan vi kan bruke det i sammenheng med `substring()`.

## Se også

- [String Substring i Kotlin Docs](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [Kotlin Regex Tutorial](https://www.programiz.com/kotlin-programming/regex)