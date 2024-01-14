---
title:    "Elm: Stor bokstavering av en streng"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang hatt en tekststreng som du ønsker å gjøre om til store bokstaver? Kanskje du ønsker å formate en tittel eller en overskrift på en mer stilig måte. Uansett årsak, er det en enkel prosedyre i Elm for å få dette til. Les videre for å lære hvordan.

## Slik gjør du det

For å kapitalisere en tekststreng i Elm, kan du bruke funksjonen `String.toUpper`. Denne funksjonen tar en tekststreng som argument og returnerer den samme teksten, men med store bokstaver i stedet for små. La oss se på et eksempel:

```Elm
String.toUpper "dette er en tekst" --> "DETTE ER EN TEKST"
```

Som du kan se, er det en enkel og direkte måte å kapitalisere en tekststreng på i Elm.

## En dypdykk i kapitalisering av strenger

Det er verdt å merke seg at når man kapitaliserer en tekststreng i Elm, vil bare de små bokstavene i alfabetet bli endret til store bokstaver. Spesialtegn og tall vil forbli uendret. For eksempel:

```Elm
String.toUpper "høst 2021!" --> "HØST 2021!"
```

En annen ting å merke seg er at hvis du prøver å kapitalisere en tekststreng som allerede inneholder store bokstaver, vil disse forbli uendret. For eksempel:

```Elm
String.toUpper "Tekst MED Både Store og Små bokstaver" --> "TEKST MED BÅDE STORE OG SMÅ bokstaver"
```

Dette er viktig å huske på når du jobber med tekststrenger i Elm.

## Se også

- Dokumentasjon for funksjonen `String.toUpper` i Elm: https://package.elm-lang.org/packages/elm/core/latest/String#toUpper
- En guide til tekstbehandling i Elm: https://guide.elm-lang.org/effects/text.html