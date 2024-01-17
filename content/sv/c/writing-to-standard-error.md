---
title:                "Skriva till standardfel"
html_title:           "C: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Writing to standard error är en viktig del av C programmering som låter oss skicka felmeddelanden och andra viktiga meddelanden till en särskild utgångskälla. Detta hjälper till att separera viktig information från den vanliga utmatningen från programmet och ger oss möjlighet att enklare felsöka vår kod.

## Så här gör du:
När du vill skriva till standard error i C, använder du funktionen `fprintf`. Ett vanligt sätt att göra det är genom att skicka ett meddelande tillsammans med standard error som destination, till exempel: 

```C
fprintf(stderr, "Det här är ett felmeddelande\n");
```

Detta skulle skriva ut "Det här är ett felmeddelande" till standard error utgången istället för den vanliga utgången. Om du vill inkludera variabler i ditt meddelande, kan du använda format specifierare, till exempel: 

```C
int num = 5;
fprintf(stderr, "Värdet på variabeln är %d", num);
```

Detta skulle skriva ut: "Värdet på variabeln är 5" till standard error utgången.

## Djupdykning:
Att skriva till standard error är viktigt för att hantera fel och viktig information vid felsökning av våra program. Innan standard error introducerades, användes ofta standard utgången för att skicka både vanliga utmatningar och felmeddelanden. Detta gjorde det svårt att hålla isär vad som var viktig information och vad som var den vanliga utmatningen.

En alternativ metod för att skicka felmeddelanden är att använda `perror` funktionen. Detta låter oss skicka ett fördefinierat felmeddelande till standard error, tillsammans med den aktuella felkoden, vilket kan vara mer användbart vid felsökning.

När vi använder `fprintf` för att skriva till standard error, använder vi en formatsträng och variabler som argument. Formatsträngen bestämmer hur utmatningen kommer att se ut och variablerna ger de faktiska värdena som ska skrivas ut.

## Se även:
Läs mer om `fprintf` funktionen och standard error i C här: https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm