---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener innebär att ändra alla tegn i strängen till små bokstäver. Programmerare gör detta för att standardisera datainmatningen och göra strängmatchning okänslig för stora och små bokstäver.

## Hur man gör:
I Fish Shell, du kan använda `string lower` för att konvertera en sträng till gemener. Här är ett enkelt exempel:

```Fish Shell
> string lower -a 'Hej VÄRLD'
hej värld
```
I ovanstående exempel konverteras strängen 'Hej VÄRLD' till gemener.

## Fördjupning
Att konvertera strängar till gemener har använts sedan datorns gryning för att förenkla textanalyser. Det finns alternativ till `string lower`, som `tr '[:upper:]' '[:lower:]'`, men `string lower` är det mest föredragna sättet i Fish Shell på grund av dess tydlighet och enkelhet. Implementeringsdetaljerna för `string lower` innebär att den itererar över varje tecken i strängen och använder Unicode-normer för att bestämma den lägre motsvarigheten för varje tecken.

## Se Även
För mer information om `string lower` och andra funktioner i Fish Shell, se följande länkar:
- Officiell Fish Shell Dokumentation: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- Stack Overflow Tråd om att konvertera strängar till gemener: [https://stackoverflow.com/questions/2264428/](https://stackoverflow.com/questions/2264428)