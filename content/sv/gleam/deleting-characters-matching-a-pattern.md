---
title:                "Att ta bort tecken som matchar ett mönster"
html_title:           "Gleam: Att ta bort tecken som matchar ett mönster"
simple_title:         "Att ta bort tecken som matchar ett mönster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att ta bort tecken som matchar ett mönster är en vanlig programmeringsteknik för att filtrera och rensa data. Genom att använda detta kan du skapa mer specifika och effektiva sökningar, samt hantera och manipulera data på ett mer precist sätt.

## Hur gör man?
För att ta bort tecken som matchar ett mönster i Gleam använder du funktionen "delete_characters_matching" tillsammans med det önskade mönstret. Exempelvis:

```Gleam
let str = "Hej Gleam!";
let result = delete_characters_matching(str, "jo");
```
Detta skulle resultera i strängen "He Glea!" eftersom båda bokstäverna j och o har tagits bort från strängen.

## Djupdykning
Att ta bort tecken som matchar ett mönster har funnits länge inom programmering och har traditionellt använts för att hantera textbaserade data. Alternativ till denna teknik inkluderar att använda reguljära uttryck eller att använda inbyggda funktioner i string-modulen i Gleam. Implementationen av funktionen "delete_characters_matching" utnyttjar både det funktionella och imperativa paradigmet för att ge en både flexibel och snabb lösning för att ta bort karaktärer i en given sträng.

## Se även
För mer information om Gleam kan du besöka den officiella hemsidan på [gleam.run](https://gleam.run/) och ansluta dig till deras community på [Slack](https://gleam-slack.herokuapp.com/). Om du vill lära dig mer om reguljära uttryck kan du ta en titt på Mozillas [guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions).