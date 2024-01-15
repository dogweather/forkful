---
title:                "Att göra en sträng stor bokstav"
html_title:           "Bash: Att göra en sträng stor bokstav"
simple_title:         "Att göra en sträng stor bokstav"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kapitalisera en sträng (göra alla bokstäver stora) är användbart när man vill förbättra läsbarheten och presentationen av en text. Det kan också vara användbart om man vill få ut data i en viss format eller om man behöver söka efter en specifik sträng som är skriven i stora bokstäver.

## Hur man gör
För att kapitalisera en sträng i Bash kan du använda inbyggda funktionen "tr". Här är ett exempel på hur du kan använda den:

```Bash
echo "hej du!" | tr 'a-z' 'A-Z'
```
Det här kommer att producera outputen "HEJ DU!". Du kan också använda regular expressions i "tr" för att specifikt välja vilka bokstäver som ska förändras. Här är ett exempel där vi bara vill göra den första bokstaven stor och resten små:

```Bash
echo "dET sKA sE oUT sÅ hÄR" | tr '[:lower:]' '[:upper:]'
```
Det här kommer att producera outputen "Det ska se ut så här". 

## Djupdykning
Det finns också andra sätt att kapitalisera en sträng i Bash, som att använda inbyggda kommandot "awk" eller "sed". Det finns också sätt att göra detta inuti ett shell script, vilket kan vara användbart om man vill förändra flera strängar på en gång. Att förstå hur dessa kommandon fungerar kan också ge dig en bättre förståelse för hur Bash hanterar strängar och format.

## Se också
Här är några ytterligare resurser som kan vara hjälpsamma för dig när du vill kapitalisera en sträng i Bash:

- [Bash tr Command](https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/)
- [Bash awk Command](https://www.geeksforgeeks.org/awk-command-unixlinux-examples/)
- [Bash sed Command](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)

Nu har du lärt dig hur du kan kapitalisera en sträng i Bash. Lycka till med dina programmeringsprojekt!