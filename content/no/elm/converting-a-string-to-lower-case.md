---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Arduino: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til lavere tilfelle (lowercase) innebærer å endre alle bokstavene i strengen til små bokstaver. Dette er ypperlig for å unngå feil ved sammeligning av strenger, siden "Elm" og "elm" behandles som forskjellige verdier uten konvertering.

## Hvordan:

Her er en enkel måte å gjøre dette på i Elm:

```Elm
import String

lowercaseString : String -> String
lowercaseString str =
    String.toLower str
```

Hvis du har strengen "ELM", vil output være "elm".

```Elm
lowercaseString "ELM"
"elm"
```

## Dypdykk:

- **Historisk kontekst:** Det har alltid vært behov for å konvertere strenger til lavere tilfelle i programmering. Funnet i mange språk som JavaScript, Java, Python og nå Elm.
- **Alternativer:** Du kan skrive din egen funksjon for å konvertere strenger til lowercase, men det er mer effektivt og mindre feilutsatt å bruke innebygde funksjoner som `String.toLower`.
- **Implementeringsdetaljer:** `String.toLower` i Elm utfører ikke bare en enkel ASCII konvertering. Den tar hensyn til unicodetegn i strengen og utfører dermed en full unicode-til-lavere-tilfelle-konvertering.

## Se Også:

For mer informasjon om strenger i Elm, sjekk ut [String - Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/String) for mer detaljert forklaring. Du kan også finne flere eksempler og brukstilfeller for `String.toLower` på [Learn Elm: String.toLower](https://www.learn-elm.org/exercise/Strings).