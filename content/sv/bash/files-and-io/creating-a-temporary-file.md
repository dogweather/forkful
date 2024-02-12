---
title:                "Skapa en temporär fil"
aliases:
- sv/bash/creating-a-temporary-file.md
date:                  2024-01-20T17:39:46.650442-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skapa en temporär fil"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapa en temporär fil innebär att du tillfälligt lagrar data som behövs under programmets körning. Programmerare använder detta för att hantera mellanlagring, undvika datarförlust vid krascher och för att hantera stora datamängder diskret.

## Så här gör du:
Använd `mktemp` för att skapa en säker temporär fil. Nedan är ett enkelt skript och exempelutdata:

```Bash
#!/bin/bash
# Skapa en temporär fil
temp_file=$(mktemp)

# Se till att den tas bort när skriptet avslutas
trap "rm -f $temp_file" EXIT

# Använd din temporära fil här
echo "Detta är ett test" > $temp_file
cat $temp_file

# Filen tas bort automatiskt här
```

Exempelutdata:
```
Detta är ett test
```

## Djupdykning:
`mktemp` kommandot introducerades för att ge en säkrare metod att hantera temporära filer än att bara använda ett förutsägbart namn, vilket ökade säkerheten. Alternativt kan du använda `tempfile` (äldre och mindre säkert) eller raka filoperatörer, men dessa metoder kan skapa säkerhetsrisker. `mktemp` skapar en unik fil i `/tmp` och undviker kollisioner och säkerhetsrisker. `trap` används för att se till att filen tas bort även om skriptet avbryts.

## Se även:
- The Open Group Base Specifications Issue 7, 2018 edition, `mktemp`: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/mktemp.html
- Bash manual för trap kommandot: https://www.gnu.org/software/bash/manual/bash.html#index-trap
