---
title:                "Søking og erstatting av tekst"
aliases:
- /no/bash/searching-and-replacing-text.md
date:                  2024-01-20T17:57:29.248051-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

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
