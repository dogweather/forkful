---
title:                "Stor bokstave en streng"
html_title:           "Haskell: Stor bokstave en streng"
simple_title:         "Stor bokstave en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å kapitalisere en streng i Haskell betyr rett og slett å gjøre den første bokstaven til en stor bokstav. Dette gjøres ofte for å gjøre teksten mer leselig eller for å følge bestemte konvensjoner i koden.

# Hvordan:
Her er en enkel funksjon som kapitaliserer en string:

```Haskell
capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
```

Eksempel på bruk av funksjonen:

```Haskell
capitalize "haskell"   -- "Haskell"
capitalize "programming"   -- "Programming"
capitalize "code"   -- "Code"
```

# Dykke dypere:
Kapitalisering av strenger er en vanlig operasjon i mange programmeringsspråk, og det er også tilgjengelig i Haskell. Denne funksjonen kan også brukes til å gjøre mer kompliserte strengmanipulasjoner, som for eksempel å gjøre alle bokstaver unntatt den første til små bokstaver.

Alternativt kan man også bruke funksjonen `toTitle` fra `Data.Char`-modulen for å kapitalisere en streng. Dette kan være nyttig hvis man trenger å håndtere spesielle tilfeller, som for eksempel akronymer eller forkortelser.

Implementasjonen av `capitalize`-funksjonen er ganske rett frem, da Haskell har innebygde funksjoner for å håndtere bokstaver, slik som `toUpper`, `toLower` og `toTitle`.

# Se også:
- [Haskell String](https://www.haskell.org/tutorial/strings.html)
- [Data.Char module](https://www.haskell.org/onlinereport/standard-prelude.html#module-data-char)