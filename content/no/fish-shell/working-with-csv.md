---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med CSV, eller "Comma-Separated Values", innebærer å manipulere og analysere data i tekstformat delt med komma. Programmerere gjør dette fordi CSV er enkelt, universelt og støttes av de fleste databehandling verktøy.

## Slik gjør du:
Å lese en CSV-fil i Fish og skrive ut noen data kan gjøres slik:

```Fish Shell
set -l filename 'data.csv'
awk -F ',' '{print $1 " " $2}' $filename
```

__Eksempel på utdata:__
```
Navn Alder
Ola 30
Kari 25
```

For å skrive til en CSV-fil:

```Fish Shell
set -l data "Per,35\nLise,28"
echo $data > nydata.csv
```

## Dypdykk
CSV-formatet ble opprettet for enkel datadeling på tvers av programmer. Det finnes alternativer som JSON og XML, men CSV er ofte foretrukket for sin enkelhet. Når man jobber med CSV i Fish, kan man bruke native Unix-verktøy som `awk`, `sed`, `cut` og `sort`. Det er viktig å håndtere forskjellige skilletegn og encodings riktig.

## Se Også
- GNU `awk` manualen: https://www.gnu.org/software/gawk/manual/gawk.html
- `sed` & `awk` 101 hacks: https://www.thegeekstuff.com/2009/12/unix-sed-tutorial-awk-101-hacks-ebook
- Fish shell dokumentasjon: https://fishshell.com/docs/current/index.html
