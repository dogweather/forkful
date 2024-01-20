---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å tolke en dato fra en streng er prosessen med å ta en tekstuell representasjon av en dato og konvertere den til en mer behandelbar form, ofte som et datatidsobjekt. Programmerere gjør dette for å manipulere, formatere, sammenligne eller beregne bygget på dato.

## Slik gjør du:

Tolking av en dato fra en streng i Bash kan oppnås ved hjelp av 'date' -kommandoen. For eksempel, la oss tolke strengen "2022-03-17":

```Bash
dato="2022-03-17"
formatert_dato=$(date -d "$dato" +'%d.%m.%Y')
echo "$formatert_dato"
```

Output:

```Bash
17.03.2022
```

## Dyp Dykk:

- Historisk Kontekst: Bash, eller Bourne Again SHell, ble skapt i 1989. Date-kommandoen har vært en av kjernefunksjonene i Bash siden begynnelsen.

- Alternativer: Skriftspråk som Python, JavaScript har innebygde funksjoner for datotolkning. Likevel, hvis du skriver et skript på servernivå eller mid-band, er Bash det mest tilgjengelige verktøyet.

- Implementasjonsdetaljer: '-d' flagget brukes til å tolke en streng til en dato. '+% d.% m.% Y' formatet tolker datoen til formatet 'dag.måned.år'.

## Se Også:

For mer informasjon om tolking av en dato fra en streng i Bash, sjekk ut disse ressursene:

- Bash Manual: [https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)
- Bash Date Command: [https://www.geeksforgeeks.org/date-command-linux-examples/](https://www.geeksforgeeks.org/date-command-linux-examples/)
- Forum Diskusjon om Bash Date Parsing: [https://stackoverflow.com/questions/3249827/parse-dates-in-bash](https://stackoverflow.com/questions/3249827/parse-dates-in-bash)