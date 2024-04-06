---
date: 2024-01-20 17:45:18.425262-07:00
description: "Hur g\xF6r man: Output."
lastmod: '2024-04-05T21:53:39.408627-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## Hur gör man:
```Bash
# Givet en sträng
full_string="Detta är ett exempel på en sträng"
# Extrahera substräng från position 6, 10 tecken lång
echo ${full_string:5:10}
```
Output:
```
är ett ex
```

```Bash
# Använda variabler för position och längd
start=5
length=10
echo ${full_string:$start:$length}
```
Output:
```
är ett ex
```

## Djupdykning:
Historiskt sett är substräng extraktion inte unik för Bash och går tillbaka till de tidiga dagarna av programmering. I Bash, infördes detta som en del av dess strängbearbetningsfunktioner. Alternativ inkluderar att använda externa verktyg som `cut`, `awk`, eller `sed`. Det som gör Bash-unikt är dess inbyggda funktionalitet utan att behöva kalla på externa kommandon, vilket effektiviserar skript. 

Detaljer i implementationen involverar viss syntax:
- `${sträng:position}` extraherar delsträng början från `position` till slutet.
- `${sträng:position:längd}` extraherar `längd` tecken början från `position`.
- Negative värden för `position` och `längd` har också stöd för omvända operationer.

## Se även:
- Bash manualen: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/string-manipulation.html
- praktisk guide till `sed`: https://www.gnu.org/software/sed/manual/sed.html
- introduktion till `awk`: https://www.gnu.org/software/gawk/manual/gawk.html
- `cut` kommandot: https://man7.org/linux/man-pages/man1/cut.1.html
