---
title:                "Bash: Radera tecken som matchar ett mönster"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att kunna ta bort tecken som matchar ett specifikt mönster är en viktig del av Bash programmering. Det kan hjälpa till att effektivisera arbetet och göra det enklare att hantera stora mängder data.

## Så här gör du

För att ta bort tecken som matchar ett visst mönster i Bash, kan du använda dig av kommandot "sed". Det står för "stream editor" och är ett kraftfullt verktyg för att manipulera textsträngar.

En enkel syntax för att ta bort tecken med "sed" är:

```Bash
sed 's/mönster//' filnamn
```

Här ersätter du "mönster" med det mönster du vill matcha och lämnar sedan det andra fältet tomt för att ta bort det matchade mönstret från filen. Du kan även lägga till flaggan "-i" för att direkt ändra i filen utan att behöva skriva ut det till en ny fil.

Låt oss säga att vi har en textfil med namnet "namn.txt" som innehåller ett antal namn, men vi vill ta bort alla namn som börjar på bokstaven "A". Vi kan då använda kommandot:

```Bash
sed 's/A//' namn.txt
```

Detta kommer att ta bort alla tecken som matchar "A" från namnen i filen och skriva ut resultatet till terminalen. Om vi istället vill spara ändringarna till samma fil kan vi lägga till flaggan "-i":

```Bash
sed -i 's/A//' namn.txt
```

Nu kommer de matchande namnen att tas bort direkt från filen.

## Djupdykning

När du använder "sed" för att ta bort tecken som matchar ett visst mönster finns det vissa saker som kan vara bra att ha i åtanke.

Först och främst är "sed" väldigt känsligt för skiljetecken. Om du använder dig av till exempel "/" i ditt mönster måste du escape:a det med en backslash ("\") för att "sed" ska förstå att det är en del av mönstret och inte ett skiljetecken.

En annan sak att tänka på är att "sed" endast kommer att ta bort det första matchande tecknet i varje rad, om inte flaggan "g" används för att göra en global sökning. Om du vill ta bort flera matchande tecken i samma rad måste du använda "g".

Det kan också vara bra att veta att "sed" är case sensitive, vilket innebär att den skiljer mellan stora och små bokstäver. Om du vill ta bort tecken oavsett om de är stora eller små kan du använda flaggan "I" för en "ignorer case" sökning.

## Se även

- [Bash sed kommando](https://blogg.vimla.se/bash-sed-kommandon/)
- [En guide till Bash](https://www.tutorialspoint.com/unix/bash/shell_variables.htm)
- [Regular Expressions Cheat Sheet (mönster)](https://www.rexegg.com/regex-quickstart.html)