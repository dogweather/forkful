---
title:                "Kontrollera om en katalog existerar"
aliases: - /sv/bash/checking-if-a-directory-exists.md
date:                  2024-02-03T19:06:42.931217-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

I Bash-programmering är det att kontrollera om en katalog finns en väsentlig kontrollmekanism som används för att verifiera närvaron av en katalog innan filoperationer utförs. Denna kontroll är avgörande för att undvika fel, såsom att försöka åtkomst eller modifiera kataloger som inte finns, vilket säkerställer smidigare och mer förutsägbar skriptkörning.

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
