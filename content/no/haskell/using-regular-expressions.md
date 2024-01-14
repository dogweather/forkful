---
title:                "Haskell: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor
Regulære uttrykk er svært nyttige verktøy for å søke og manipulere tekst i Haskell. Ved å bruke regulære uttrykk, kan du enkelt finne, erstatte og validere tekst som følger et spesifikt mønster. Dette er spesielt nyttig for å analysere store datasett eller behandle brukerinput i programmer.

## Slik gjør du det
For å bruke regulære uttrykk i Haskell, må du først importere modulen Text.Regex og deretter lage en regex-verdi ved å bruke funksjonen makeRegex. Her er et enkelt eksempel på å finne alle forekomster av tallet 42 i en streng:

```Haskell
import Text.Regex

regex = makeRegex "42" :: Regex
str = "Det er 42 ulver i skogen."

match = matchRegex regex str :: Maybe String

-- Output: Just "42"
```

Du kan også bruke regulære uttrykk til å erstatte tekst ved å bruke funksjonen subRegex. For eksempel, hvis du vil bytte ut alle forekomster av bokstaven "a" med tallet 7 i en streng:

```Haskell 
import Text.Regex

regex = makeRegex "a" :: Regex
str = "Amanda er en apekatt."

replacedStr = subRegex regex str "7" :: String

-- Output: "7m7nd7 er en 7pek7tt."
```

Du kan også bruke regulære uttrykk til å validere brukerinput. For eksempel, hvis du bare vil tillate tall i et brukernavn:

```Haskell
import Text.Regex

regex = makeRegex "^[0-9]+$" :: Regex
username = "1234"

isValid = matchTest regex username :: Bool

-- Output: True
```

## Dypdykk
Regulære uttrykk bruker et spesielt språk for å definere mønstre. Det finnes mange forskjellige symboler og operatorer som kan brukes til å lage mer komplekse uttrykk. Du kan lese mer om disse symbolene og hvordan de fungerer i Haskell-dokumentasjonen for Text.Regex-modulen.

En viktig ting å huske på når du bruker regulære uttrykk er at de kan være krevende for datamaskinen å behandle. Hvis du jobber med store datasett eller komplekse mønstre, kan det være lurt å vurdere å bruke en parser i stedet for regulære uttrykk for å unngå potensielle ytelsesproblemer.

## Se også
- [Haskell-dokumentasjon for Text.Regex](https://hackage.haskell.org/package/regex/docs/Text-Regex.html)
- [En oversikt over regulære uttrykk](https://www.regular-expressions.info/)
- [En seksjon om regulære uttrykk i Learn You a Haskell for Great Good](http://learnyouahaskell.com/starting-out#regular-expressions)