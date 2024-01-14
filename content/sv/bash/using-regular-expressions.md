---
title:    "Bash: Användning av reguljära uttryck"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Varför använda reguljära uttryck?

Reguljära uttryck är ett kraftfullt verktyg för mönstermatchning och sökning i text. Genom att använda reguljära uttryck kan man effektivt filtrera och manipulera stora mängder data. Det kan även vara användbart vid validering av användarinput i form av text eller för att extrahera önskad information från en textfil.

## Så här använder du reguljära uttryck i Bash

För att använda reguljära uttryck i Bash behöver du använda kommandot `grep`. Detta kommando används för att söka efter en viss sträng i en fil eller från en annan kommandorads output. Här är ett exempel på hur man kan söka efter alla filer i ett visst format, till exempel alla filer som slutar på ".txt":

```Bash
grep "\.txt$" filer.txt
```

Detta kommer att söka igenom filen "filer.txt" och endast returnera namnen på filer som slutar med ".txt". Låt oss nu anta att vi vill söka efter alla telefonnummer som är skrivna enligt ett visst format i en textfil. Vi kan använda reguljära uttryck för att göra detta:

```Bash
grep "\([0-9]\{3\}\) [0-9]\{3\}-[0-9]\{4\}" kontakter.txt
```

Detta kommer att returnera alla rader i filen "kontakter.txt" som innehåller telefonnummer i formatet "(XXX) XXX-XXXX". De \ tecknen innan siffrorna indikerar att det är ett reguljärt uttryck och de siffror som är inom klamrarna visar antalet tecken som ska matchas.

## Djupdykning i reguljära uttryck

Reguljära uttryck kan verka komplicerade till en början, men de kan snabbt bli ett oumbärligt verktyg när du lär dig grunderna. Förutom att söka och filtrera data i filer kan man också manipulera data med hjälp av reguljära uttryck. Detta kan göras med hjälp av kommandot `sed`, som står för "stream editor". Sed använder sig av reguljära uttryck för att ändra och manipulera dataströmmar.

Här är ett exempel på hur man kan använda `sed` för att ersätta ett visst ord i en textfil:

```Bash
sed 's/bra/dålig/g' textfil.txt
```

Detta kommando kommer att ersätta alla förekomster av ordet "bra" med ordet "dålig" i filen "textfil.txt". Genom att använda reguljära uttryck i kombination med andra Bash-kommandon kan man utföra komplexa manipulationer på stora mängder data.

## Se även

- [10 Bash-kommandon som alla bör kunna](https://www.tutorialspoint.com/unix_commands/10-bash-commands-every-linux-user-should-know.htm)
- [Bash-snarvägar för effektiviserad programmering](https://coderwall.com/p/o0gmvw/50-efficient-bash-shortcuts)