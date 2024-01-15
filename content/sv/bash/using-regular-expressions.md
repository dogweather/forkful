---
title:                "Användning av reguljära uttryck"
html_title:           "Bash: Användning av reguljära uttryck"
simple_title:         "Användning av reguljära uttryck"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
Regular expressions är ett kraftfullt verktyg för att söka och manipulera textmönster i en textfil eller ett command line interface. Genom att lära sig hur man använder regelbundna uttryck kan du spara tid och effektivisera din kodning.

## Hur man gör
För att använda regelbundna uttryck i Bash behöver du för det första lära dig syntaxen för dessa uttryck. Gå sedan igenom några praktiska exempel för att få en bättre förståelse för hur de fungerar.

Först måste vi veta hur man väljer ett sökord eller ett mönster. För att göra det använder vi tecknet ```=` som betyder att vi vill extrahera en sträng. Här är ett exempel på ett regelbundet uttryck för att hitta en sträng som innehåller både bokstäver och siffror i en textfil:

```Bash
grep '[a-z0-9]' textfil.txt
```

Då kommer alla rader i textfilen som innehåller både bokstäver och siffror att visas. Om vi istället vill hitta en sträng som börjar med en siffra kan vi använda tecknet ```^``` före siffran:

```Bash
grep '^1' textfil.txt
```

Detta kommer att visa alla rader i textfilen som börjar med siffran 1. Det finns också flera andra specialtecken som kan användas för att söka efter mer specifika mönster.

## Djupdykning
För att bli mer bekant med regelbundna uttryck är det en bra idé att titta på dokumentationen för Bash så att du lär dig alla tillgängliga alternativ och specialtecken. Här är en kort förklaring av några av de vanligaste specialtecknen:

- ```*``` betyder att det föregående tecknet kan förekomma 0 eller flera gånger.
- ```+``` betyder att det föregående tecknet måste förekomma minst en gång.
- ```?``` betyder att det föregående tecknet är valfritt.
- ```.``` betyder vilken som helst enskild karaktär.
- ```[...]``` betyder en serie av annars okända tecken.

Det finns också mycket mer avancerade regelbundna uttryck som du kan utforska för att ta din kodning till nästa nivå. Genom att förstå regelbundna uttryck kan du effektivt hantera stora mängder data och uppnå önskat resultat på ett snabbt och enkelt sätt.

## Se också
- [Bash-dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [RegEx-tutorial](https://www.regular-expressions.info/tutorial.html)
- [Praktiska exempel på regelbundna uttryck i Bash](https://www.linuxjournal.com/content/bash-extended-globbing)