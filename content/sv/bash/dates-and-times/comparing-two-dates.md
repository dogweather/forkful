---
date: 2024-01-20 17:32:20.277521-07:00
description: "Hur man g\xF6r: Exempeloutput."
lastmod: '2024-04-05T21:53:39.432992-06:00'
model: gpt-4-1106-preview
summary: ''
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

## Hur man gör:
```Bash
# Jämför två datum
datum1="2023-03-17"
datum2="2023-04-15"

# Omvandla till sekunder sedan epoch (1970-01-01 00:00:00 UTC)
sek1=$(date -d "$datum1" +%s)
sek2=$(date -d "$datum2" +%s)

# Jämför sekunderna
if [ "$sek1" -eq "$sek2" ]; then
  echo "Dagen är den samma!"
elif [ "$sek1" -lt "$sek2" ]; then
  echo "$datum1 är tidigare än $datum2"
else
  echo "$datum1 är senare än $datum2"
fi
```
Exempeloutput:
```
2023-03-17 är tidigare än 2023-04-15
```

## Djupdykning:
Att jämföra datum är viktigt, speciellt före internets genomslag. Då sorterade man pappersdokument för hand. I skriptvärlden kan du använda `date`-kommandot för mycket av tunglyftningen. Det finns andra verktyg som `awk` eller `perl`, men `date` är ofta tillräckligt kraftfullt och finns standard i de flesta Unix-system, inklusive macOS.

Implementationen beror på formatet. `date`-kommandot är flexibelt men kräver korrekt format. Om du har olika datumformat eller använder andra tidzoner blir det svårare – då kan du behöva andra kommandon eller skripta det själv.

## Se även:
- Man-sidor för `date` med `man date` i terminalen.
- Bash Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Stack Overflow för specifika problem och lösningar: https://stackoverflow.com/questions/tagged/bash
