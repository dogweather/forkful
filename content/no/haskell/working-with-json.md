---
title:                "Haskell: Arbeide med json"
simple_title:         "Arbeide med json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du er en Haskell-programmerer, har du sannsynligvis hørt om JSON før. JSON står for JavaScript Object Notation, og det er et populært format for å lagre og utveksle data mellom forskjellige programmeringsspråk. Så hvorfor bør du interessere deg for å jobbe med JSON i Haskell? En grunn kan være for å kunne samhandle med APIer som bruker JSON-formatet, noe som er vanlig i moderne webutvikling. Det kan også være nyttig når du jobber med databehandling og ønsker å lagre eller dele data på tvers av programmer.

## Hvordan

Haskell har innebygde funksjoner for å håndtere JSON-data, noe som gjør det enkelt å jobbe med dette formatet. La oss se på noen eksempler på hvordan du kan bruke disse funksjonene:

```Haskell
import Data.Aeson

-- Konverterer en Haskell-verdi til JSON
encode "Hei verden" 
-- Output: "\"Hei verden\""

-- Konverterer JSON til en Haskell-verdi
decode "\"Hei verden\"" :: Maybe String 
-- Output: Just "Hei verden"

-- Oppretter en JSON-verdi fra en liste med tuples
toJSON [("Navn", "Maria"), ("Alder", 30), ("Land", "Norge")] 
-- Output: Object (fromList [("Navn",String "Maria"),("Alder",Number 30.0),("Land",String "Norge")])
```

Som du kan se, kan Haskell enkelt håndtere konvertering mellom JSON og Haskell-verdier, samt å opprette JSON-verdier fra en liste med tuples. Det finnes også mange nyttige funksjoner for å manipulere og gjøre operasjoner på JSON-data, som du kan utforske videre på egen hånd.

## Dypdykk

Hvis du ønsker å lære mer om hvordan Haskell håndterer JSON, kan du se nærmere på pakken "aeson". Denne pakken tilbyr en rekke funksjoner og verktøy for å jobbe med JSON-data i Haskell, inkludert marshalling og unmarshalling av komplekse datastrukturer, validering av JSON-data og mye mer.

En annen nyttig ressurs er Learning Haskell Data Analysis, som blant annet tar for seg hvordan man kan bruke Haskell og JSON for å analysere data og bygge datadrivne applikasjoner.

## Se også

- [Data.Aeson dokumentasjon](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html)
- [Haskell for datanalyse](https://www.packtpub.com/big-data-and-business-intelligence/haskell-data-analysis)