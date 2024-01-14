---
title:                "Elm: Omgjøring av en streng til små bokstaver"
simple_title:         "Omgjøring av en streng til små bokstaver"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en streng til små bokstaver kan være nyttig når du ønsker å sammenligne tekst på en enkel måte. Dette er spesielt nyttig når du arbeider med søkefunksjoner eller når du vil søke i en tekstfil eller database.

## Hvordan gjøre det
For å konvertere en streng til små bokstaver i Elm, kan vi bruke funksjonen `String.toLower`. La oss se på et eksempel:

```Elm
stringToLower : String -> String
stringToLower string =
  String.toLower string
```

Her har vi en funksjon som tar inn en streng og bruker `String.toLower` for å returnere samme streng i små bokstaver. La oss nå prøve å kjøre denne funksjonen med noen eksempler og se på resultatet:

```Elm
stringToLower "HEI"
-- gir som output "hei"

stringToLower "Elm programmering"
-- gir som output "elm programmering"
```

Som du kan se, så har funksjonen vår konvertert alle bokstavene til små bokstaver.

## Dypdykk
Det er verdt å merke seg at denne funksjonen kan håndtere ikke bare engelske bokstaver, men også andre språk som bruker diakritiske tegn. Det betyr at den også vil konvertere for eksempel "å" til "a" eller "ø" til "o". Dette er en viktig egenskap å være klar over når du jobber med flerspråklige applikasjoner.

En annen ting å huske på er at funksjonen `String.toLower` vil returnere en ny streng i små bokstaver, men den vil ikke endre den opprinnelige strengen. Dette betyr at hvis du ønsker å konvertere en streng til små bokstaver og deretter bruke den i en sammenligning, så må du sørge for å lagre resultatet av funksjonen i en ny variabel.

## Se også
- Offisiell dokumentasjon for `String.toLower` i Elm: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- En guide til Elm programmering på norsk: https://lamstack.nu/elm/guide/