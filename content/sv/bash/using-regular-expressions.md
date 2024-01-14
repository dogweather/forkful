---
title:                "Bash: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför

Reguljära uttryck är ett kraftfullt verktyg inom Bash-programmering som gör det möjligt att söka, matcha och manipulera text på ett enkelt och effektivt sätt. Genom att lära sig att använda reguljära uttryck, kan du spara tid och arbete när du arbetar med strängar och filer inom Bash.

## Hur man använder reguljära uttryck i Bash

För att använda reguljära uttryck i Bash behöver du först förstå den grundläggande syntaxen och reglerna för hur de fungerar. För att söka och matcha ett uttryck i en text måste du använda kommandot `grep` tillsammans med ett reguljärt uttryck. Här är ett exempel på hur man söker efter alla ord som börjar med bokstaven "s" i en textfil:

```Bash
cat textfil.txt | grep '^s'
```
Output:
```
sopa
sol
sjukhus
```
För att söka efter flera bokstäver eller ord kan du använda hakparenteser [] för att skapa en mönstergrupp. Till exempel, om du vill söka efter alla ord som börjar med antingen "hej" eller "håll" kan du använda följande reguljära uttryck:

```Bash
cat textfil.txt | grep '^(hej|håll)'
```
Output:
```
hejda
hej
hållbart
hållplats
```

Det går också att använda reguljära uttryck för att ersätta text i en fil. Du kan göra det med hjälp av kommandot `sed` och det reguljära uttrycket inom en sista grupp. Här är ett exempel på hur man ersätter alla instanser av orden "god morgon" med "hej" i en textfil:

```Bash
sed -i 's/god morgon/hej/g' textfil.txt
```

Det finns många fler möjligheter att använda reguljära uttryck inom Bash-programmering. Genom att lära dig mer om dess syntax och användningsområden kan du effektivisera ditt arbete och lösa problem på ett smidigare sätt.

## Djupdykning i reguljära uttryck

Reguljära uttryck är en del av POSIX-standard och används i många olika programmeringsspråk och verktyg. Inom Bash finns dock vissa begränsningar och skillnader i hur de tolkas jämfört med andra språk. Till exempel, så måste man använda en omvänd snedstreck innan specialtecken såsom +, (), {} för att tolkas korrekt.

Det finns också olika metakaraktärer som kan användas för olika syften i reguljära uttryck. Till exempel, används ^ för att matcha början av en rad, $ för att matcha slutet, * för att matcha noll eller flera gånger av ett uttryck och + för att matcha en eller flera gånger. Mer information om olika metakaraktärer och hur de kan användas finns tillgängligt i Rusells bok "The GNU Linux Command Line". 

## Se även

- [The GNU Linux Command Line](http://www.linuxcommand.org/tlcl.php)
- [Bash Regex tutorial](https://ryanstutorials.net/regular-expressions-tutorial/)
- [Bash Quick Reference Guide](http://tldp.org/LDP/abs/html/x9644.html)