---
title:                "Läsning av kommandoradsargument"
html_title:           "Bash: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Läsning av kommandoradargument i Bash är ett sätt för programmerare att få sin kod att interagera med användarinput från kommandoraden. Det är en viktig del av att skriva interaktiva Bash-skript som kan anpassas efter användarens önskemål.

## Så här gör du:
Att läsa kommandoradargument i Bash är enkelt. Du behöver bara använda variabeln $1 för det första argumentet, $2 för det andra argumentet och så vidare. Här är ett exempel på hur du kan använda detta i din kod:

```Bash
#!/bin/bash
# Skapa en variabel med första argumentet
arg1=$1

# Skriv ut värdet på variabeln
echo "Det första argumentet är: $arg1"
```

Om vi kör det här skriptet och ger "Hej" som första argument, kommer output att bli:
```
Det första argumentet är: Hej
```

## Djupdykning:
Att läsa kommandoradargument i Bash är inte något nytt påfund. Det är ett vanligt sätt för programmerare att tillåta användarinput i skript och program. En alternativ metod är att använda "read" kommandot för att läsa användarinput direkt från terminalen. Men detta kan vara opraktiskt om du behöver läsa flera argument samtidigt.

När du läser kommandoradargument, är det viktigt att hålla i åtanke att argumenten separeras av mellanslag. Om du behöver läsa argument som innehåller mellanslag, måste du använda citationstecken runt argumentet när du kallar på skriptet eller kommandot.

## Se även:
[The Bash Manual](https://www.gnu.org/software/bash/manual/bash.html) - En utförlig guide till Bash-programmering. 
[Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial) - En grundläggande handledning för att komma igång med Bash-skriptning.