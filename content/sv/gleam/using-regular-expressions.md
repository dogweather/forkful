---
title:                "Gleam: Att använda reguljära uttryck"
simple_title:         "Att använda reguljära uttryck"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Varför
I denna artikel kommer vi att utforska användningen av reguljära uttryck inom Gleam programmering. Reguljära uttryck är ett kraftfullt verktyg för att söka och manipulera textsträngar och kan användas för att lösa olika problem inom programmering. Låt oss ta en titt på hur de fungerar och hur de kan hjälpa dig i dina Gleam-projekt.

## Så här gör du
För att använda reguljära uttryck inom Gleam behöver du först importera det relevanta biblioteket "re" med hjälp av följande kod:
```Gleam
import re
```
Du kan sedan använda funktionen "match" för att hitta en matchning av ett reguljärt uttryck i en textsträng. Till exempel kan du använda följande kod för att hitta en matchning av ett telefonnummer i en textsträng:
```Gleam
let str = "Ring mig på 070-12345678"
re.match("^07[0-9]{8}", str) 
```
Detta skulle ge utskrift:
```Gleam
Ok[ {Match.begin: 12, Match.end: 23} ]
```
Du kan också använda reguljära uttryck för att söka och ersätta delar av en sträng. Till exempel kan du använda följande kod för att byta ut ett telefonnummer i en sträng med ett annat:
```Gleam
let str = "Mitt telefonnummer är 070-12345678"
re.replace("^07([0-9]{8})", "08$1", str) 
```
Detta skulle ge utskrift:
```Gleam
Ok "Mitt telefonnummer är 08-12345678"
```
Det finns många fler funktioner och användningsområden för reguljära uttryck inom Gleam, så tveka inte att utforska och experimentera med dem i dina egna projekt.

## Djupdykning
Reguljära uttryck följer ett speciellt syntax, vilket kan verka förvirrande i början. Men när du väl förstår hur de fungerar kan de vara mycket användbara. Ett vanligt användningsområde för reguljära uttryck är att validera inmatningsdata, till exempel för att kontrollera att ett lösenord har en viss längd eller innehåller en viss typ av tecken. Det finns också många olika specialtecken och operatorer som du kan använda för att bygga mer komplexa reguljära uttryck.

## Se också
- [Gleam dokumentation för reguljära uttryck](https://gleam.run/modules/re.html)
- [Gleam reguljära uttryck exempelkod](https://github.com/gleam-lang/gleam_stdlib/blob/main/test/re_test.gleam)