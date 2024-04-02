---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:49.382736-07:00
description: "\xC5 arbeide med CSV-filer (Comma-Separated Values) i Bash inneb\xE6\
  rer \xE5 behandle og manipulere tabelldata lagret i vanlig tekstformat. Dette er\
  \ essensielt for\u2026"
lastmod: '2024-03-13T22:44:40.996992-06:00'
model: gpt-4-0125-preview
summary: "\xC5 arbeide med CSV-filer (Comma-Separated Values) i Bash inneb\xE6rer\
  \ \xE5 behandle og manipulere tabelldata lagret i vanlig tekstformat. Dette er essensielt\
  \ for\u2026"
title: Arbeide med CSV
weight: 37
---

## Hva & Hvorfor?
Å arbeide med CSV-filer (Comma-Separated Values) i Bash innebærer å behandle og manipulere tabelldata lagret i vanlig tekstformat. Dette er essensielt for programmerere da det muliggjør automatisering av oppgaver relatert til datatransformasjon, analyse og integrasjon direkte fra kommandolinjen, uten behov for tyngre verktøy eller programmeringsmiljøer.

## Hvordan:

**Lese en CSV-fil Linje for Linje**

```bash
while IFS=, read -r kolonne1 kolonne2 kolonne3
do
  echo "Kolonne 1: $kolonne1, Kolonne 2: $kolonne2, Kolonne 3: $kolonne3"
done < eksempel.csv
```

*Eksempel på utskrift:*

```
Kolonne 1: id, Kolonne 2: navn, Kolonne 3: epost
...
```

**Filtrere CSV-rader Basert på en Betingelse**

Ved å bruke `awk`, kan du enkelt filtrere rader. For eksempel, for å finne rader hvor andre kolonne er lik "Alice":

```bash
awk -F, '$2 == "Alice" { print $0 }' eksempel.csv
```

**Endre en Kolonneverdi**

For å endre den andre kolonnen til store bokstaver:

```bash
awk -F, 'BEGIN {OFS=",";} { $2 = toupper($2); print $0; }' eksempel.csv
```

**Sortere en CSV-fil Basert på en Kolonne**

Du kan sortere en CSV-fil basert på, si, den tredje kolonnen (numerisk):

```bash
sort -t, -k3,3n eksempel.csv
```

**Bruke `csvkit` til Mer Komplekse Oppgaver**

`csvkit` er en samling av kommandolinjeverktøy for å konvertere til og arbeide med CSV. Det kan installeres via pip.

For å konvertere en JSON-fil til CSV:

```bash
in2csv data.json > data.csv
```

For å spørre en CSV-fil ved bruk av SQL:

```bash
csvsql --query "SELECT name FROM eksempel WHERE id = 10" eksempel.csv
```

*Merk: Installasjon av `csvkit` krever Python og kan gjøres ved å bruke `pip install csvkit`.*
