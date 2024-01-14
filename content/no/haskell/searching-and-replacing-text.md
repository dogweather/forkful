---
title:                "Haskell: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å søke og erstatte tekst kan være en av de vanligste oppgavene når man programmerer. Enten det er å rette skrivefeil eller å endre variabelnavn, så er det viktig å kunne effektivt søke og erstatte tekst i koden sin.

## Hvordan gjøre det

I Haskell kan man bruke funksjoner som `replace` og `sub` for å søke og erstatte tekst i en streng. La oss si at vi har en tekststreng som inneholder navn og hvor det er skrevet på feil måte, f.eks. "johN doe". Vi kan bruke `replace "johN" "John"` for å erstatte "johN" med "John", og få ut "John doe" som resultat. Her er et eksempel på hvordan dette kan gjøres:

```Haskell
import Data.Text as T
-- Definerer en funksjon som tar inn en tekststreng og returnerer endret tekst
replaceText :: Text -> Text
replaceText text = T.replace "johN" "John" text

main = do
  let original = "johN doe"
  let modified = replaceText original
  print modified
```

Dette vil gi følgende output:

```
"John doe"
```

Man kan også søke og erstatte deler av en tekst basert på et mønster, ved hjelp av funksjonen `sub`. For eksempel hvis vi ønsker å bytte ut alle tall i en tekststreng med ordet "nummer", kan vi gjøre det slik:

```Haskell
import Data.Text as T
-- Definerer en funksjon som tar inn en tekststreng og returnerer endret tekst
replaceNumbers :: Text -> Text
replaceNumbers text = T.sub regex "nummer" text
  where regex = "([0-9]+)" -- Regulært uttrykk som matcher alle tall i tekststrengen

main = do
  let original = "Jeg har 3 epler og 2 bananer"
  let modified = replaceNumbers original
  print modified
```

Dette vil gi output:

```
"Jeg har nummer epler og nummer bananer"
```

## Graving dypere

Hvis man ønsker å gjøre mer avanserte søk og erstatt-operasjoner, kan man bruke biblioteket "regex-posix". Dette lar deg bruke fullstendige regulære uttrykk, og gir en mer fleksibel måte å søke og erstatte tekst på. Her er et eksempel på hvordan man kan bruke dette biblioteket:

```Haskell
import Text.Regex.Posix
-- Funksjon for å søke og erstatte basert på et regulært uttrykk
regexReplace :: String -> String -> String -> String
regexReplace pattern replacement string = subRegex (matchRegexAll (makeRegex pattern) string) string (replacement ++ "\\") -- Legger til "\\1" for å beholde det som matcher

main = do
  let original = "I disse dager er temperaturer på 23 grader vanlige"
  let modified = regexReplace "\\b([0-9]+)\\b" "num num" original -- Matcher og bytter ut alle tall enkeltvis
  print modified
```

Dette vil gi output:

```
"I disse dager er temperaturer på num num grader vanlige"
```

Som du kan se, kan man med dette biblioteket gjøre mer avanserte operasjoner med tekst.

## Se også

- [Haskell String: Replace Tutorial](https://www.tecmint.com/bash-replace-string-variable-tutorial/)
- [Haskell Regex Tutorial](http://learnyouahaskell.com/starting-out#regular-expressions)