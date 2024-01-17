---
title:                "Extrahering av substrängar"
html_title:           "C: Extrahering av substrängar"
simple_title:         "Extrahering av substrängar"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Detta handlar om att extrahera delsträngar i en C-programmering. Det är en vanlig teknik som används för att åtkomst till och manipulera delar av en större sträng. Programmerare använder det för att enkelt hämta information från en sträng utan att behöva bläddra igenom hela strängen.

## Hur man gör:
Här är ett enkelt exempel på hur man extraherar en delsträng från en större sträng i C:

```c
// Skapa en sträng
char str[] = "Välkommen till C-programmering";

// Extrahera en delsträng från index 10 till index 19
char substr[10];
int start = 10;
int end = 19;

// Kopiera delsträngen till substr
strncpy(substr, str + start, end-start);
substr[end-start] = '\0';

// Skriv ut delsträngen
printf("Delsträngen är: %s", substr);
```

Detta skulle producera följande utmatning:

`Delsträngen är: C-programmer`

Notera att vi använder funktionen `strncpy()` för att kopiera delsträngen och lägger till en nollbyte för att markera slutet på delsträngen.

## Djupdykning:
Extrahera delsträngar är inte ett nytt koncept och har funnits i många år. I C-programmering finns också andra sätt att utföra samma uppgift, såsom att använda `memcpy()` eller löpande `for` -loopar för att kopiera tecken efter tecken. Beroende på applikationen kan en metod vara snabbare eller mer lättläst än den andra.

När det gäller implementationen av extrahering av delsträngar, måste programmören se till att de angivna indexen inte leder till några minnesfel, som att försöka extrahera en delsträng som är längre än den ursprungliga strängen. Det är alltid bra att använda inbyggda funktioner som `strlen()` för att säkerställa att de angivna indexen är inom strängens gränser.

## Se även:
- [strncpy()](https://www.cplusplus.com/reference/cstring/strncpy/)
- [memcpy()](https://www.cplusplus.com/reference/cstring/memcpy/)
- [strlen()](https://www.cplusplus.com/reference/cstring/strlen/)