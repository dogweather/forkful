---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:42.931217-07:00
description: "Hur man g\xF6r: I grunden till\xE5ter Bash dig att kontrollera om en\
  \ katalog existerar med hj\xE4lp av villkorssatser och `-d`-operatorn. Nedan f\xF6\
  ljer ett rakt p\xE5\u2026"
lastmod: '2024-03-13T22:44:38.096547-06:00'
model: gpt-4-0125-preview
summary: "I grunden till\xE5ter Bash dig att kontrollera om en katalog existerar med\
  \ hj\xE4lp av villkorssatser och `-d`-operatorn."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur man gör:
I grunden tillåter Bash dig att kontrollera om en katalog existerar med hjälp av villkorssatser och `-d`-operatorn. Nedan följer ett rakt på sak exempel som demonstrerar hur man utför denna kontroll.

```bash
if [ -d "/path/to/directory" ]; then
    echo "Katalogen finns."
else
    echo "Katalogen finns inte."
fi
```

Exempelutskrift (om katalogen finns):
```
Katalogen finns.
```

Exempelutskrift (om katalogen inte finns):
```
Katalogen finns inte.
```

För mer komplexa skript är det vanligt att kombinera kontrollen med andra operationer, såsom att skapa katalogen om den inte finns:

```bash
DIR="/path/to/directory"
if [ -d "$DIR" ]; then
    echo "$DIR finns."
else
    echo "$DIR finns inte. Skapar nu..."
    mkdir -p "$DIR"
    echo "$DIR skapad."
fi
```

Exempelutskrift (om katalogen inte finns och sedan skapas):
```
/path/to/directory finns inte. Skapar nu...
/path/to/directory skapad.
```

Även om Bash själv tillhandahåller robusta verktyg för sådana kontroller, finns det inga populära tredjepartsbibliotek specifikt för denna uppgift, eftersom inbyggda Bash-kommandon är fullt kapabla och effektiva för validering av katalognärvaro.
