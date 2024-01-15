---
title:                "Bruke regelmessige uttrykk"
html_title:           "Elm: Bruke regelmessige uttrykk"
simple_title:         "Bruke regelmessige uttrykk"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med tekstbehandling eller dataanalyse, er sjansene store for at du har støtt på behovet for å finne spesifikke mønstre eller uttrykk i teksten din. Dette er hvor regular expressions (regex) kommer inn i bildet. Ved å bruke regex kan du søke og manipulere tekst på en effektiv og nøyaktig måte.

## Hvordan

En måte å bruke regular expressions i Elm er gjennom Regex-pakken, som kan installeres ved hjelp av Elm Package Manager. La oss si at vi ønsker å finne alle telefonnumre i en tekststreng, og deretter formatere dem med parenteser rundt landskoden. Her er et eksempel på hvordan du kan gjøre det:

```Elm
import Regex

tekst = "Ring oss på +47 12345678 eller +47 87654321 for mer informasjon!"

pattern = Regex.regex "\\+\\d{2} \\d{8}"

resultat = Regex.find pattern tekst |> Maybe.map Regex.matches |> Maybe.withDefault []

formaterNummer nummer =
    "+47 " ++ nummer

mapper formaterNummer resultat
```

Output: `["+47 12345678", "+47 87654321"]`

La oss gå gjennom koden. Først importerer vi Regex-pakken. Deretter definerer vi en tekststreng som vi ønsker å finne telefonnummer i. Vi definerer også et regex-mønster som matcher det formatet vi er ute etter. Vi bruker `Regex.find` for å finne alle matcher av mønsteret i teksten vår, og deretter mapper vi funksjonen `formaterNummer` på hver match for å formatere det som ønsket. Til slutt får vi en liste over alle matchende telefonnumre i riktig format.

## Dypdykk

Regex er en kraftig teknikk for å håndtere tekst, men det kan også være utfordrende å lære. Det er mange forskjellige operatorer og spesielle symboler som kan brukes til å bygge mønstre, og det kan ta litt tid å bli vant til dem. Heldigvis finnes det mange ressurser på nettet for å hjelpe deg i gang. En av de beste er RegExr, en interaktiv regex editor som lar deg eksperimentere med ulike mønstre og se hvordan de blir matchet mot tekst.

Du kan også ta en titt på elm-regex-paketet på GitHub for å få en dypere forståelse av hvordan Regex fungerer i Elm. Det er også verdt å merke seg at Regex er en del av et større konsept innenfor tekstbehandling kalt regulære språk, så hvis du ønsker å grave enda dypere, finnes det mange ressurser om dette også.

## Se også

- [RegExr](https://regexr.com/)
- [Elm Regex-pakken](https://package.elm-lang.org/packages/elm/regex/latest/)
- [GitHub: elm-regex](https://github.com/elm-explorations/regex)