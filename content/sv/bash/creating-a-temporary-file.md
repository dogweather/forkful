---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Skapa tillfälliga filer i Bash innebär att generera filer för kortvarig användning. Programmerare gör detta för att lagra data som tas bort när filen är stängd eller scriptet avslutas.

## Så här gör du:

Här är ett exempel på hur du skapar och använder en tillfällig fil i Bash.

```Bash
#!/bin/bash

# Skapa en tillfällig fil
tmpFile=$(mktemp)

# Skriv något till filen
echo "Hej världen" > $tmpFile

# Läs filinnehållet
cat $tmpFile

# Ta bort filen
rm $tmpFile

```

Ditt output blir:

```Bash
Hej världen
```

## Djup Dykning

1. **Historiska Kontext**: Funktionen `mktemp` kom med ISO POSIX (2001) och har varit tillgänglig i Bash sedan dess.
2. **Alternativ**: Förutom `mktemp` finns det andra kommandon som `tempfile` eller direkt användning av `$$` eller `$RANDOM` variabeln för att skapa tillfälliga filer.
3. **Implementation Detaljer**: `mktemp` garanterar att filen skapas på ett säkert sätt. Det hindrar också flera processer från att skapa samma fil samtidigt.

## Se Även

- Bash Programming Guide: https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
- Bash `mktemp` Man Page: https://www.man7.org/linux/man-pages/man1/mktemp.1.html