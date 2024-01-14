---
title:    "Elm: Konvertere en streng til små bokstaver."
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor

Å konvertere en streng til små bokstaver er en vanlig oppgave når du jobber med tekstbehandling eller databehandling. Dette kan være nyttig for å sikre enhetlig formatering eller for å sammenligne tekststrukturer.

# Hvordan

For å konvertere en streng til små bokstaver i Elm, kan du bruke funksjonen "String.toLower". Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Elm
import String

name = "ELM ER KULT"

toLower = String.toLower name

-- Output: "elm er kult"
```

Dette koden vil konvertere den opprinnelige strengen "ELM ER KULT" til små bokstaver og lagre den i en ny variabel med navnet "toLower". Du kan deretter bruke denne nye variabelen i koden din.

# Dykk dypere

Når du bruker funksjonen "String.toLower", er det viktig å være oppmerksom på at den bare konverterer bokstaver fra det engelske alfabetet til små bokstaver. Hvis du har tekst på andre språk, som for eksempel norsk, kan det bli feil i konverteringen. Det kan også føre til uønskede resultater hvis det er spesielle tegn eller symboler i strengen.

En annen ting å være oppmerksom på er at funksjonen bare konverterer bokstaver, ikke tall eller andre tegn. Hvis du trenger å konvertere hele strengen, kan du bruke funksjonen "String.toLowercase", som også inkluderer tall og spesielle tegn.

# Se også

- Offisiell dokumentasjon for "String.toLower" funksjonen: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Elm Guide: https://guide.elm-lang.org/
- Elm Discuss Forum: https://discourse.elm-lang.org/