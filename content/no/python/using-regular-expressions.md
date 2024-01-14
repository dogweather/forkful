---
title:                "Python: Å bruke regulære uttrykk"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang sittet fast i å finne et bestemt ord eller en frase i en stor tekstfil? Med regulære uttrykk kan du enkelt finne og erstatte tekstbaserte mønstre i Python. Det kan være en nyttig ferdighet for å effektivisere din kode og få deg ut av slike frustrerende situasjoner.

## Hvordan

For å kunne bruke regulære uttrykk i Python må du først importere `re` biblioteket. Du kan deretter bruke funksjoner som `match()`, `search()` og `findall()` for å søke etter og finne matcher for bestemte mønstre i en streng.

```python
import re

tekst = "Hei, mitt navn er Nora. Jeg elsker å kode."

# Bruk match() for å finne ord som starter med "e"
print(re.match(r"e\w+", tekst)) 
# Output: None

# Bruk search() for å finne første forekomst av "el"
print(re.search(r"el", tekst)) 
# Output: <_sre.SRE_Match object; span=(18, 20), match='el'>

# Bruk findall() for å finne alle forekomster av "e" etterfulgt av 2 bokstaver
print(re.findall(r"e\w{2}", tekst)) 
# Output: ['els', 'eld', 'erk']

```

## Dypdykk

Regulære uttrykk tar i bruk spesielle symboler og syntaks for å lage mønstre som kan matche tekst. For eksempel kan `.` brukes for å matche alle tegn, mens `+` brukes for å matche et eller flere forekomster av tegn.

Det er også mulig å bruke ulike flagg som `IGNORECASE` for å ignorere store/små bokstaver og `DOTALL` for å inkludere linjeskift i matchen.

En annen nyttig funksjon er bruk av grupper i regulære uttrykk. Dette lar deg finne og lagre spesifikke deler av matchen til senere bruk.

## Se også

- [The Python Tutorial: Regular Expressions](https://docs.python.org/3/tutorial/regex.html)
- [Regex Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/python)
- [Python Regular Expression Examples](https://www.programiz.com/python-programming/regex)