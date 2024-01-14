---
title:                "Gleam: Radera tecken som matchar ett mönster"
simple_title:         "Radera tecken som matchar ett mönster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

I detta blogginlägg kommer vi att titta på hur man kan använda Gleam för att ta bort tecken som matchar ett visst mönster från en sträng. Detta kan vara användbart för att rensa upp data och förbereda det för vidare bearbetning.

## Hur man gör

För att ta bort tecken som matchar ett visst mönster från en sträng kan du använda funktionen `String.replace` i Gleam. Funktionen tar tre argument: strängen där matchningen ska ske, det mönster som man vill ta bort och vad den matchade strängen ska ersättas med. Här är ett exempel på hur man kan använda funktionen:

```Gleam

import gleam/string

let input = "Detta är en teststräng som innehåller en viss typ av text som vi vill ta bort"
let output = String.replace(input, "viss typ av text", "")

```

I detta exempel kommer `output` att vara "Detta är en teststräng som innehåller en som vi vill ta bort". Det vill säga, den delen av strängen som matchar mönstret har blivit borttagen.

## Djupdykning

För mer avancerade behov kan man använda sig av reguljära uttryck för att matcha och ta bort tecken från en sträng. Det finns flera funktioner i Gleam som gör det möjligt att använda reguljära uttryck, bland annat `Regexp.replace` och `Regexp.replace_all`. Dessa funktioner tar också tre argument: strängen där matchningen ska ske, det reguljära uttrycket som man vill matcha och vad den matchade strängen ska ersättas med.

Låt oss ta ett exempel där vi vill ta bort alla siffror från en sträng:

```Gleam

import gleam/regexp

let input = "123 Detta är en teststräng som innehåller siffror"
let output = Regexp.replace_all(input, "[0-9]", "")

```

I detta fall kommer `output` att vara "Detta är en teststräng som innehåller siffror".

Det är viktigt att notera att reguljära uttryck kan vara rätt så förvirrande för nybörjare, så det kan kräva lite övning innan du känner dig bekväm med att använda dem. Men de kan vara väldigt användbara när man vill göra mer avancerade matchningar och ersättningar.

## Se även

- Gleam dokumentation om `String.replace`: https://gleam.run/book/libraries/std.string.html#replace
- Gleam dokumentation om reguljära uttryck: https://gleam.run/book/libraries/std.regexp.html