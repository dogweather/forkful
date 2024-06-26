---
date: 2024-01-20 17:39:46.650442-07:00
description: "S\xE5 h\xE4r g\xF6r du: Anv\xE4nd `mktemp` f\xF6r att skapa en s\xE4\
  ker tempor\xE4r fil. Nedan \xE4r ett enkelt skript och exempelutdata."
lastmod: '2024-03-13T22:44:38.101382-06:00'
model: gpt-4-1106-preview
summary: "Anv\xE4nd `mktemp` f\xF6r att skapa en s\xE4ker tempor\xE4r fil."
title: "Skapa en tempor\xE4r fil"
weight: 21
---

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
