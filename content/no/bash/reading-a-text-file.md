---
title:                "Lese en tekstfil"
html_title:           "Bash: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Lesing av tekstfiler er en vanlig oppgave for programmerere. Det innebærer å lese en tekstfil, som kan inneholde forskjellige typer data og informasjon, og behandle den på en måte som er nyttig for programmet vårt. Dette kan være nødvendig for å utføre en bestemt oppgave, hente informasjon fra en database eller for å behandle brukerinndata.

## Hvordan:

```Bash
# Les en tekstfil med navn "tekstfil.txt"
cat tekstfil.txt

# Les en tekstfil og lagre den i en variabel
tekst=$(cat tekstfil.txt)

# Les en tekstfil line by line og lagre hver linje i en variabel
while read line; do
  echo "Linje: $line"
done < tekstfil.txt
```

Eksempel på output:

```
Dette er en tekstfil.
Den inneholder litt tekst.
Vi kan lese den med Bash.
```

## Dykk dypere:

Lesing av tekstfiler har vært en viktig del av programmering i lang tid, helt siden den første datamaskinen ble utviklet. I dag finnes det flere alternative måter å lese tekstfiler på, for eksempel med andre programmeringsspråk eller ved hjelp av tredjepartsprogrammer. Implementeringsdetaljer og syntaks kan også variere avhengig av hvilket operativsystem man bruker.

## Se også:

- [Linux Kommandoer: Cat](https://www.linux.no/wiki/Kommandoer/cat)
- [Bash Guide: Lesing av filer](https://mywiki.wooledge.org/BashGuide/InputAndOutput#Reading_files)