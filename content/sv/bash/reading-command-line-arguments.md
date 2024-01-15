---
title:                "Att läsa kommandoradsargument"
html_title:           "Bash: Att läsa kommandoradsargument"
simple_title:         "Att läsa kommandoradsargument"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför

Att läsa in och använda kommandoradsargument kan vara väldigt användbart för att automatisera uppgifter och snabbt utföra olika operationer på filer eller program. Det är också ett sätt att anpassa och effektivisera din terminalupplevelse.

## Hur man gör det

För att läsa in och använda kommandoradsargument i Bash, används variabler som representerar de argument som användaren skriver in vid körningen av skriptet. Dessa variabler används sedan som input till kommandon och skript som utförligen tolkar och utför åtgärder baserade på värdet av argumenten.

```Bash
#!/bin/bash

# Ett enkelt skript som visar användning av kommandoradsargument
echo "Hej $1, välkommen till världen av Bash-skript!"
echo "Jag heter $2 och är din guide."
```

Om detta skript sparas som "welcome.sh" och sedan körs med följande kommando: ```./welcome.sh Johan Skript```, kommer utmatningen att vara: ```Hej Johan, välkommen till världen av Bash-skript! Jag heter Skript och är din guide.``` I detta fall är kommandoradsargumenten "Johan" och "Skript" tilldelade till variablerna $1 och $2, vilket används i skriptet.

## Djupdykning

Det finns flera olika sätt att läsa in och använda kommandoradsargument i Bash, inklusive att använda tonoptioner som "-f" eller "-h". Det är också möjligt att processa argument med hjälp av loopen "for" och hantera argument med flaggor i en logisk ordning. Det är också viktigt att hantera felhantering och validering av argument för att undvika ogiltiga eller potentiellt farliga operationer.

Se även:

-[The Beginner's Guide to Bash Scripting](https://www.linode.com/docs/guides/beginners-guide-to-bash-scripting/)
-[Bash Guide for Beginners](http://tldp.org/LDP/Bash-Beginners-Guide/html/index.html)