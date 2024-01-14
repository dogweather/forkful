---
title:                "Elm: Å gjøre en streng stor bokstav"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er ofte enkle oppgaver som å kapitalisere en streng i programvareutvikling som kan føre til frustrasjon når man ikke vet hvordan man skal løse dem. I denne bloggposten vil vi lære hvordan vi enkelt kan kapitalisere en streng i programmeringsspråket Elm.

## Hvordan

For å kapitalisere en streng i Elm, kan vi bruke den innebygde funksjonen `String.capitalize` som tar inn en streng og returnerer en kopi av strengen med første bokstav gjort stor.

``` Elm
String.capitalize "hei, jeg heter maria" -- Output: "Hei, jeg heter maria"
```

Hvis vi ønsker å kapitalisere alle ordene i en streng, kan vi bruke funksjonen `String.words` for å splitte strengen inn i en liste med ord, og deretter bruke `String.capitalize` på hvert enkelt ord. Deretter kan vi slå sammen den nye listen med ord tilbake til en streng ved hjelp av `String.join`.

``` Elm
String.join " " (List.map String.capitalize (String.words "hei, jeg heter maria")) -- Output: "Hei, Jeg Heter Maria"
```

## Dypdykk

Det er viktig å merke seg at `String.capitalize` fungerer på enkelte bokstaver som ikke er en del av det engelske alfabetet, men ikke på alle. Dette skyldes at funksjonen gjør antagelser basert på unicode-karakterene.

For mer avanserte behov, som å kapitalisere kun en del av en streng eller inkludere spesielle bokstaver, kan det være nyttig å bruke en tredjeparts pakke som `elm-community/string-extra`.

## Se også

- Offisiell Elm dokumentasjon for `String` modulen: https://package.elm-lang.org/packages/mdgriffith/elm-bytes-extra/latest/String