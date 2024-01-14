---
title:                "Fish Shell: Radering av tecken som matchar ett mönster"
simple_title:         "Radering av tecken som matchar ett mönster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster är en användbar funktion inom Fish Shell som kan hjälpa dig att effektivisera ditt program. Detta kan till exempel vara användbart om du behöver rensa bort oönskade tecken i ett dokument eller om du vill manipulera text på ett smidigt sätt.

## Så här gör du

För att ta bort tecken som matchar ett visst mönster kan du använda kommandot `string replace`. Detta kommando tar tre argument: det första är mönstret som ska matchas, det andra är det tecken som ska ersättas och det tredje är den befintliga texten som du vill manipulera. Här är ett exempel på hur du kan använda det:

```Fish Shell
string replace hund katt "jag äger en hund"
```

I detta fall kommer kommandot att byta ut alla förekomster av ordet "hund" med ordet "katt" i textsträngen "jag äger en hund". Detta skulle ge följande output:

```
jag äger en katt
```

Du kan även använda reguljära uttryck för att matcha ett mer komplicerat mönster. Till exempel kan du använda `.*` för att matcha alla tecken före och efter ett visst ord. Här är ett exempel på hur du kan använda det:

```Fish Shell
string replace ".* äger en" "har en" "jag äger en hund"
```

Detta skulle ge följande output:

```
har en hund
```

## Utforska djupare

För mer komplexa användningar kan du använda `string sub` istället för `string replace`. Detta kommando har möjlighet att ta emot en lista av reguljära uttryck och ersättningssträngar, vilket ger dig ännu mer kontroll över vilka tecken som ska manipuleras och hur de ska ersättas. Du kan läsa mer om detta på Fish Shells dokumentationsida.

## Se också

1. Fish Shell - https://fishshell.com
2. Fish Shell dokumentation - https://fishshell.com/docs/current/cmds.html#string-cmds
3. Reguljära uttryck - https://en.wikipedia.org/wiki/Regular_expression