---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Strenginterpolering i Bash: En Praktisk Guide

## Hva & Hvorfor?
Strenginterpolering er en måte å injisere variabler direkte inn i en streng på. Programmerere bruker det fordi det gjør det enkelt å konstruere komplekse strenger uten å måtte ty til konkatenering.

## Hvordan gjøre det:
Se på koden under. Her viser vi hvordan å interpolere en streng med variabel i Bash:

```Bash
navn="Ole Brum"
hilsen="Hei, jeg heter $navn. Hyggelig å møte deg!"
echo $hilsen
```

Når du kjører denne koden, vil resultatet bli:

```Bash
Hei, jeg heter Ole Brum. Hyggelig å møte deg!
```

## Dypdykk
Strenginterpolering har vært en del av programmeringsspråk siden tiden til C. Alternativt kunne du bruker printf-funksjonen i Bash for å oppnå tilsvarende resultat, men det krever litt mer syntax enn å bare bruke en dollar `$`.

For eksempel:

```Bash
printf "Hei, jeg heter %s. Hyggelig å møte deg!" "$navn"
```

Dette kan være nyttig hvis du vil kontrollere formatet på utdataene dine på en mer detaljert måte.

## Se også
For mer hjelp og informasjon om Bash og strenginterpolering, sjekk ut følgende lenker:

1. The Bash Guide: [Variable Substitution](https://guide.bash.academy/variables/#variable-substitution)
2. GNU Bash Manual: [Shell Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)