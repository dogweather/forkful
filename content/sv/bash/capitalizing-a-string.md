---
title:                "Att göra en sträng versal"
date:                  2024-01-19
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att göra en textsträng versal innebär att konvertera alla bokstäver till stora bokstäver. Programmerare gör detta för att standardisera textdata, förbättra läslighet eller uppfylla tekniska krav.

## Hur gör man:
```Bash
# Kapitalisera en sträng med tr-kommandot
echo "hej världen" | tr '[:lower:]' '[:upper:]'
```
Output: HEJ VÄRLDEN

```Bash
# Använda Bash inbyggda funktioner för att kapitalisera en sträng
str="hej världen"
echo "${str^^}"
```
Output: HEJ VÄRLDEN

## Fördjupning
Från jokertecken i UNIX till dagens Bash har textmanipulering alltid varit centralt. `tr` är ett äldre kommando som står för "translate" och modifierar strömmar av text. Bash introducerade senare inbyggda funktioner för strängmanipulering, som `${str^^}`, som gör koden enklare och snabbare.

Alternativ kan vara att använda `awk`, `sed`, eller till och med Perl och Python för komplex bearbetning. Varje verktyg har sina för- och nackdelar, beroende på behov och miljö.

Implementationen av strängkapitalisering skiljer sig i skalkriptning. Det beror på vilket skal som används och dess version. I Bash 4.0 och senare är ${str^^} och ${str,,} inbyggda för att respektive konvertera till versaler eller gemener.

## Se även
- Bash manualen: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- POSIX specifikationen för `tr`: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/tr.html
