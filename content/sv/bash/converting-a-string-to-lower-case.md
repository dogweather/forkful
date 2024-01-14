---
title:                "Bash: Konvertera en sträng till gemener"
simple_title:         "Konvertera en sträng till gemener"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Varför

Att konvertera en sträng till gemener (lower case) kan vara användbart i många olika situationer inom Bash-programmering. Det kan göra det enklare att jämföra strängar, söka efter specifika delar av en sträng eller helt enkelt göra det mer läsvänligt.

##Så här gör du

Att konvertera en sträng till gemener i Bash är enkelt med hjälp av kommandot "tr" (translate). Syntaxen för detta kommando är "tr [OLD] [NEW]", där [OLD] är de tecken du vill byta ut och [NEW] är de tecken du vill byta ut dem med. I vårt fall vill vi byta ut alla versala (upper case) tecken i strängen med motsvarande gemena tecken. Så här ser det ut i praktiken:

```Bash
echo "DETTA ÄR EN STRÄNG" | tr '[:upper:]' '[:lower:]'
```

Det första steget är att skriva ut strängen (echo) och sedan pipa resultatet till tr-kommandot. I [OLD] anger vi "[:upper:]", vilket betyder att vi vill byta ut alla versala tecken. I [NEW] anger vi "[:lower:]", vilket betyder att vi vill ersätta dem med motsvarande gemena tecken. Om vi kör detta kommer utmatningen (output) att vara "detta är en sträng" - nu har alla versala tecken omvandlats till gemena.

Ett annat sätt att konvertera en sträng till gemener är med hjälp av "sed" (stream editor). I detta fall använder vi sed för att göra en global ersättning av alla versala tecken till gemena tecken i vår sträng. Så här kan det se ut:

```Bash
echo "DETTA ÄR EN STRÄNG" | sed 's/[A-Z]/[a-z]/g'
```

Det första steget är samma som tidigare, vi skriver ut strängen och pipar den till sed-kommandot. I [s/OLD/NEW/] anger vi vilken ersättning vi vill göra, och med "g" markerar vi att vi vill göra detta globalt (alltså för alla förekomster av versala tecken i strängen).

##Djupdykning

Nu när vi vet hur vi kan konvertera en sträng till gemener i Bash, låt oss titta lite närmare på vad som faktiskt händer bakom kulisserna. När vi använder kommandot "tr" byter vi egentligen ut tecknen baserat på deras ASCII-värden. Så varje versalt tecken (t.ex. A) har ett numeriskt värde, det är detta värde som tr använder för att byta ut det korrekta gemena tecknet (i vårt fall a). Detta sker tack vare tabellen "charmap" som finns i Bash.

Med "sed" använder vi en regelbaserad ersättning (substitution) för att byta ut tecken. Detta innebär att vi alltså anger en regel för vad som ska bytas ut mot vad, istället för att bara använda ASCII-värden. Detta ger oss mer flexibilitet och möjlighet att göra mer avancerade ersättningar.

##Se även

- tr Command i Bash: https://www.tutorialspoint.com/unix_commands/tr.htm
- sed Command i Bash: https://www.tutorialspoint.com/unix_commands/sed.htm
- ASCII-tabellen: https://en.wikipedia.org/wiki/ASCII