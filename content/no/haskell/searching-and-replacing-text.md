---
title:    "Haskell: Søke og erstatte tekst"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Hvorfor

Utilsiktet feilskriving eller endring av tekst kan være en vanlig utfordring i programmering. Å søke og erstatte denne teksten kan være en tidkrevende og kjedelig oppgave. Med Haskell sin kraftige funksjonelle programmeringsstil kan du effektivt håndtere denne oppgaven og spare tid og krefter.

# Hvordan gjøre det

For å søke og erstatte tekst i Haskell, kan du bruke funksjonene `find` og `replace` fra `Data.Text` biblioteket. For eksempel, hvis du ønsker å erstatte alle forekomster av "hej" med "hei" i en tekststreng `str`, kan du bruke følgende kode:

```Haskell
import Data.Text as T
import Data.Text.Replace as TR

str = "Hej, verden!"
replace "hej" "hei" str
```

Dette vil gi deg utgangsresultatet `Hei, verden!`. Du kan også bruke `find` og `replace` funksjonene på filer ved å bruke `readFile` og `writeFile` funksjonene.

En annen nyttig funksjon er `Regex.replace`, som lar deg erstatte tekst basert på et regex-uttrykk. For eksempel, hvis du ønsker å erstatte alle tall med "X" i en tekststreng `str`, kan du bruke følgende kode:

```Haskell
import Text.Regex as R

str = "123 abc 456"
R.replace (R.mkRegex "\\d+") str "X"
```

Dette vil gi deg utgangsresultatet `X abc X`.

# Dypdykk

Selv om det å søke og erstatte tekst med Haskell er enkelt, er det viktig å være oppmerksom på at funksjonene `find` og `replace` er case-sensitive. Dette betyr at hvis du vil erstatte en tekststreng som er skrevet i forskjellige store og små bokstaver, må du først gjøre hele strengen til en av de to. Du kan bruke funksjonene `toLower` og `toUpper` fra `Data.Text` biblioteket for dette.

En annen viktig ting å huske på er at `Regex.replace` funksjonen bare erstatter den første forekomsten av regex-uttrykket i en tekststreng. For å erstatte alle forekomster må du bruke funksjonen `Regex.subRegex` istedenfor.

# Se også

- [Data.Text biblioteket](https://hackage.haskell.org/package/text)
- [Text.Regex biblioteket](https://hackage.haskell.org/package/regex)