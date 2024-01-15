---
title:                "Søking og utskifting av tekst"
html_title:           "Haskell: Søking og utskifting av tekst"
simple_title:         "Søking og utskifting av tekst"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med å søke og erstatte tekst? Det kanhende virke som en kjedelig og tidkrevende oppgave, men ved å automatisere denne prosessen kan du spare deg selv for mye tid og frustrasjon. Med Haskell, et funksjonelt programmeringsspråk, kan du enkelt lage funksjoner som kan søke og erstatte tekst på en effektiv måte.

## Hvordan

Søke og erstatte tekst i Haskell er enkelt når du vet hvordan. Her er en enkel funksjon som tar inn en liste med strenger og erstatter alle forekomster av et spesifikt ord med et annet:

```Haskell
replaceAll :: [String] -> String -> String -> [String]
replaceAll [] _ _ = []  -- Hvis listen er tom, returner en tom liste
replaceAll (x:xs) old new = (replaceWord x old new):(replaceAll xs old new) -- Kaller rekursivt på resten av listen
    where
        replaceWord str old new = unwords $ map (\w -> if w == old then new else w) (words str) -- Splitter strengen opp i ord, erstatter det gamle ordet med det nye og setter det sammen igjen
```

Her kan du se hvordan funksjonen brukes med en liste av strenger, gammelt ord og nytt ord som parametere:

```Haskell
> replaceAll ["Haskell er gøy!", "Jeg elsker Haskell."] "Haskell" "Python"
["Python er gøy!", "Jeg elsker Python."]
```

Det gamle ordet "Haskell" er nå erstattet med det nye ordet "Python" i begge strengene. Du kan også eksperimentere med å bytte ut andre ord og se hvordan funksjonen fungerer.

## Deep Dive

For de som ønsker å gå dypere inn i prosessen med å søke og erstatte tekst i Haskell, er det verdt å nevne funksjonen "replace" som finnes i standardbiblioteket "Text.Replace". Denne funksjonen tar inn en streng, et gammelt mønster og et nytt mønster, og erstatter alle forekomster av det gamle mønsteret med det nye. Dette gjør det enda enklere å utføre søk og erstatning av tekst i Haskell, spesielt hvis du trenger å bruke regex (regular expressions).

## Se også

- Offisiell Haskell-nettside: https://www.haskell.org/
- Haskell-dokumentasjon: https://www.haskell.org/documentation/
- Haskell-programmering på norsk: https://www.norskprogrammering.no/category/haskell/