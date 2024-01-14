---
title:                "Haskell: Å søke og erstatte tekst"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi har alle vært der - du skriver en stor kodebase, og plutselig innser du at du har brukt feil variabelnavn overalt. Eller, enda verre, du bytter navn på en funksjon og må manuelt endre hver eneste forekomst av den i koden din. Heldigvis finnes det en enklere måte å gjøre dette på - ved å søke og erstatte tekst i Haskell.

## Hvordan

Den grunnleggende syntaksen for å søke og erstatte i Haskell er ganske enkel. Du først angi hva du vil søke etter, og deretter hva du vil erstatte det med, ved hjelp av `substitute`-funksjonen i `Text.Regex`.

Her er et eksempel på hvordan du kan bytte ut alle forekomster av "kat" med "hund" i en streng:

```Haskell
import Text.Regex

main = do
  let string = "I love my cat, she's the best"
  let newString = subRegex (mkRegex "cat") string "dog"

  putStrLn newString
```

Dette vil produsere følgende utgang:

```Haskell
I love my dog, she's the best
```

## Dypere dykk

Du kan også bruke regulære uttrykk i søke- og erstatningsprosessen. Dette åpner for mye mer avanserte søkemønstre, og gir deg større fleksibilitet når du søker etter og erstatter tekst.

For eksempel, hvis du vil bytte ut alle tall i en streng med "#", kan du bruke følgende uttrykk:

```Haskell
let newString = subRegex (mkRegex "[0-9]+") string "#"
```

Dette vil erstatte alle tall (uavhengig av antall sifre) med "#". Du kan også bruke spesialkarakterer som `+`, `*` og `?` for å gjøre enda mer komplekse søkemønstre.

## Se også

- [Haskell's `Text.Regex` dokumentasjon](https://hackage.haskell.org/package/regex-compat-0.99.1/docs/Text-Regex.html)
- [En guide til regulære uttrykk i Haskell](https://wiki.haskell.org/Regular_expressions)