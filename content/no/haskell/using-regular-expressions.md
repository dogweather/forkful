---
title:                "Bruke regulære uttrykk."
html_title:           "Haskell: Bruke regulære uttrykk."
simple_title:         "Bruke regulære uttrykk."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor
Du har kanskje hørt om regular expressions før, men lurt på hvorfor du burde lære deg det? Vel, her er to gode grunner: det er et kraftig verktøy for å håndtere tekstbehandling og det er en essensiell ferdighet for å forstå og bruke mange programmeringsspråk, inkludert Haskell.

## Slik gjør du det
Hvis du vil lære deg regular expressions i Haskell, er det enkelt! Du trenger bare å bruke funksjonene `=~` og `=~~` for å søke gjennom og matche tekst. La oss se et eksempel på hvordan det fungerer:

```Haskell
import Text.Regex.Posix

-- Matcher alle tall i en streng og returnerer en liste av matcher
matchTall :: String -> [String]
matchTall s = getAllTextMatches (s =~ "[0-9]+") 

-- Returnerer True hvis en streng inneholder et primtall, ellers False
erPrimtall :: String -> Bool
erPrimtall s = s =~~ "[1-9]+"
``` 

**Eksempel input:** "Det er 27 primtall i mengden av de 100 første naturlige tallene."\
**Forventet output:** `["27", "100"]` og `False` 

Som du kan se, kan du bruke regular expressions til å finne og manipulere tekst på en enkel måte.

## Dypdykk
Hvis du vil lære mer om regular expressions, er det mange ressurser der ute som kan hjelpe deg. En god start er å se på dokumentasjonen til Text.Regex.Posix modulen i Haskell. Det finnes også mange nettsider som tilbyr interaktive regex-trenere, hvor du kan prøve ut forskjellige uttrykk og se hvordan de fungerer.

## Se også
- [Haskell Tutorial: Strings and Characters](https://www.haskell.org/tutorial/strings.html)
- [Regular-Expressions.info: A website for learning, testing, and reference](https://regular-expressions.info/)