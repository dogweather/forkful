---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att interpolera en sträng innebär att infoga variablernas värden inom en sträng direkt. Programmerare gör detta för att skapa dynamisk kod, effektivisera utskriften och förbättra kodläsbarheten.

## Så här gör du:
Vårt exempel kommer att använda echo-kommandot för att demonstrera stränginterpolation i Bash. 

```Bash
#!/bin/bash
name="Stockholm"
echo "Jag bor i $name"
```
Kör detta skript och du kommer att se följande utdata:

```Bash
Jag bor i Stockholm
```

## Djupdykning
Stränginterpolation i Bash går tillbaka till skapandet av Unix-skriptspråken. Det är en standardfunktion och föredras framför att använda konkatenation. Det finns liknande funktioner i andra skriptspråk som Python och Ruby. Införandet av stränginterpolation förbättrade effektiviteten i kodningen och minskade komplexiteten. 

Alternativen till stränginterpolation inkluderar användning av "expr" eller 'printf'-funktionen. Men dessa kan vara mer komplicerade och mindre lättlästa än stränginterpolation.

När det gäller implementeringsdetaljer, när en variabel interpoleras i en sträng, byter Bash automatiskt ut variabelnamnet med dess värde. Det kräver inte några extra resurser och påverkar därmed inte programmets prestanda. 

## Se också 
För mer information om Bash och stränginterpolation, se följande länkar: 

1. [Bash Beginners Guide](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_03_03.html)
2. [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/string-manipulation.html)
   
Notera: för att läsa artiklarna ovan behöver du engelska kunskap.