---
title:    "Bash: Användning av reguljära uttryck"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

Bash programmering: Användning av reguljära uttryck

## Varför

Reguljära uttryck är ett kraftfullt verktyg inom Bash-programmering. Genom att använda reguljära uttryck kan man effektivt söka och manipulera textsträngar, vilket är en mycket användbar funktion inom skriptning. Reguljära uttryck kan också förbättra läsbarheten och effektiviteten i ens kod.

## Hur man använder reguljära uttryck i Bash

Det första steget för att använda reguljära uttryck är att förstå syntaxen. Ett reguljärt uttryck består av specialtecken och metatecken som tillsammans bildar en mall för att matcha eller ersätta text. Det finns många olika specialtecken och metatecken men några av de vanligaste är:

- `. `: Matchar ett enda tecken
- `*`: Matchar 0 eller flera av de föregående tecknen
- `+`: Matchar 1 eller flera av de föregående tecknen
- `[]`: Definierar en möjlig karaktäruppsättning att välja från
- `^`: Matchar början av en rad
- `$`: Matchar slutet av en rad

För att använda reguljära uttryck i Bash, måste man använda kommandot `grep`. Detta kommando söker igenom en textsträng efter en matchning till det angivna reguljära uttrycket och returnerar raderna som innehåller matchningen. Det finns också andra användbara kommandon som `sed` och `awk` för att manipulera text med hjälp av reguljära uttryck.

Här är ett exempel på hur man kan använda reguljära uttryck för att hitta filer i en mapp som matchar ett visst mönster:

```Bash
ls | grep '^bild.*\.jpg$'
```
Detta kommando listar alla filer i mappen som börjar med "bild" och slutar med ".jpg".

## Djupdykning

Reguljära uttryck kan också användas för att ersätta text eller för att extrahera specifika delar av en textsträng. Detta finns i form av grupper, vilket är en del av uttrycket som är markerat med parenteser. Grupper kan sedan refereras till med hjälp av speciella variabler som `$1`, `$2`, etc.

Här är ett exempel på hur man kan använda reguljära uttryck för att extrahera ett telefonnummer från en textsträng:

```Bash
echo "Kontakta mig på 070-1234567!" | grep -o '[0-9]\{3\}-[0-9]\{7\}'
```

Detta kommer att returnera "070-1234567" som matchar mönstret för ett vanligt telefonnummer.

## Se även

- [En introduktion till reguljära uttryck för Bash](https://ryanstutorials.net/regular-expressions-tutorial/)
- [Grep man-sida](https://linux.die.net/man/1/grep)
- [Sed och Awk introduktionsguide](https://www.tutorialspoint.com/unix_commands/sed.htm)