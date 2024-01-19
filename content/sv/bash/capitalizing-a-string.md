---
title:                "Gör en sträng versal"
html_title:           "Bash: Gör en sträng versal"
simple_title:         "Gör en sträng versal"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kapitalisera en sträng innebär att omvandla dess första tecken till stort. Programmerare gör detta för att förbättra läsbarheten och formaliteten i deras kod, eller för att uppfylla vissa kodkonventioner.

## Hur man gör:
I Bash kan du kapitalisera en sträng genom funktionen `tr`. Följande är ett exempel:

```Bash
str="hej världen"
echo "${str^}"
```

Resultatet blir:

```Bash
Hej världen
```

Det första bokstaven i strängen har nu kapitaliserats.


## Djupdykning
Denna förmåga att kapitalisera en sträng introducerades först i Bash 4.0. Innan dessa funktioner lades till, behövde Bash-programmerare använda fler verktyg som `awk` eller `sed` för att uppnå samma resultat.

Ett alternativ till `${str^}` kan vara att använda `tr`-kommandot för att explicit byta ut små till stora bokstäver. Dock ska du vara medveten om att detta kommer att omvandla alla bokstäver i strängen till stora och inte bara det första tecknet.

För att se till att bara det första tecknet kommer att kapitaliseras, finns det ingen implementation i standardbiblioteket; funktionen `${str^}` tar hand om att erbjuda denna funktionalitet.

## Se även
För mer information, se följande länkar:

- GNU Bash Manual: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- Stack Overflow discussion on string capitalization in Bash: https://stackoverflow.com/questions/2264428/how-to-convert-a-string-to-lower-case-in-bash
- Bash String Manipulation Guide: http://tldp.org/LDP/abs/html/string-manipulation.html