---
title:                "Arbeid med CSV"
date:                  2024-01-19
html_title:           "Bash: Arbeid med CSV"
simple_title:         "Arbeid med CSV"

category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Arbeid med CSV (Comma-Separated Values) innebærer å håndtere tekstfiler organisert ved at verdier skilles med komma. Programmerere gjør dette fordi CSV er et enkelt og utbredt format for utveksling av data mellom ulike systemer.

## Hvordan:
```Bash
# Leser en CSV-fil linje for linje
while IFS=, read -r kolonne1 kolonne2
do
  echo "Kolonne 1: $kolonne1 - Kolonne 2: $kolonne2"
done < input.csv

# Skriver til en CSV-fil
echo "verdi1,verdi2" > output.csv

# Sorterer en CSV-fil etter den andre kolonnen
sort -t, -k2,2 input.csv
```
Output eksempel:
```
Kolonne 1: eple - Kolonne 2: 5
Kolonne 1: banan - Kolonne 2: 7
```

## Dykk Dypt
CSV-formatet har ingen standardisering, men det er i praksis fra det tidlige 1970-tallet. Alternativer inkluderer JSON, XML og SQLite. Når du arbeider med CSV i Bash, bruk kommandoer som `cut`, `awk`, `sort`, og `grep` for å bearbeide dataene.

## Se Også
- [GNU Coreutils Manual](https://www.gnu.org/software/coreutils/manual/coreutils.html)
- [CSV på Wikipedia](https://no.wikipedia.org/wiki/CSV)
- [Bash Programming Guide](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
