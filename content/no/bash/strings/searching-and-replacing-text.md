---
date: 2024-01-20 17:57:29.248051-07:00
description: "S\xF8k og erstatt er prosessen med \xE5 finne tekststrenger og bytte\
  \ dem ut med noe annet. Programmerere bruker dette for \xE5 oppdatere kode, korrigere\
  \ feil,\u2026"
lastmod: '2024-03-13T22:44:40.957828-06:00'
model: gpt-4-1106-preview
summary: "S\xF8k og erstatt er prosessen med \xE5 finne tekststrenger og bytte dem\
  \ ut med noe annet."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

## What & Why?
Søk og erstatt er prosessen med å finne tekststrenger og bytte dem ut med noe annet. Programmerere bruker dette for å oppdatere kode, korrigere feil, eller endre data på en effektiv måte.

## How to:
Bruk `sed` for å søke og erstatte i en tekstfil. Her er et enkelt eksempel:

```Bash
echo "Hallo verden" | sed 's/verden/Norge/'
```

Output:

```
Hallo Norge
```

Flere filer? Intet problem. For å endre alle forekomster av "http" til "https" i flere filer:

```Bash
sed -i 's/http/https/g' *.html
```

Legg merke til `-i` som lagrer endringene tilbake i filene.

## Deep Dive
Unix-verktøyet `sed`, kort for *stream editor*, ble introdusert på 1970-tallet med editorerne ed og ex. Det står sterk fortsatt fordi det er kraftfullt, og kommandoer kan skriptes.

Alternativer inkluderer `awk` som er kraftig for tekstbehandling og `perl` som er et helt programmeringsspråk egnet til komplekse tekstoperasjoner.

Implementasjonsdetaljer: `sed` fungerar ved å lese en fil linje for linje, bearbeide den med gitt kommando, og skrive ut til standard output. `-i` flagget endrer filen på plass, men bruk med forsiktighet!

## See Also
- GNU `sed` manualen: https://www.gnu.org/software/sed/manual/sed.html
- `awk` tutorial: https://www.gnu.org/software/gawk/manual/gawk.html
- Perl dokumentasjon: https://perldoc.perl.org/
