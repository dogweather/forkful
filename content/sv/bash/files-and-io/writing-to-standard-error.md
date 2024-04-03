---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:53.845583-07:00
description: "Hur man g\xF6r: I Bash anv\xE4nder du `>&2` f\xF6r att omdirigera utdata\
  \ till stderr. H\xE4r \xE4r ett grundl\xE4ggande exempel."
lastmod: '2024-03-13T22:44:38.098467-06:00'
model: gpt-4-0125-preview
summary: "I Bash anv\xE4nder du `>&2` f\xF6r att omdirigera utdata till stderr."
title: Skriva till standardfel
weight: 25
---

## Hur man gör:
I Bash använder du `>&2` för att omdirigera utdata till stderr. Här är ett grundläggande exempel:

```bash
echo "Detta är ett normalt meddelande"
echo "Detta är ett felmeddelande" >&2
```

Att köra detta skript kommer att visa båda meddelandena i konsolen, men om du omdirigerar dem kan du separera stdout från stderr. Till exempel:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` kommer att innehålla `"Detta är ett normalt meddelande"`, medan `error.txt` kommer att fånga `"Detta är ett felmeddelande"`.

För ett praktiskt användningsfall, tänk dig ett skript som behandlar filer och rapporterar ett fel om en fil inte finns:

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename finns inte!" >&2
    exit 1
else
    echo "Bearbetar $filename"
fi
```

Exempel på utdata direkt i konsolen när `example.txt` inte finns:

```
example.txt finns inte!
```

Det finns inga direkt till tredjepartsbibliotek i Bash för att hantera stderr, eftersom omdirigering är inbyggt stödd och generellt tillräckligt. Dock, för komplexa applikationer, kan loggningsramverk eller externa loggningsverktyg som `syslog` eller `log4bash` införlivas för att hantera både stdout och stderr mer effektivt.
