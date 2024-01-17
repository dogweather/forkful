---
title:                "Extrahera delsträngar"
html_title:           "Gleam: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substränger är processen att ta en del av en sträng, vanligtvis baserat på en viss position eller längd, och returnera det som en separat sträng. Programerare gör detta för att hantera väldigt långa strängar eller för att manipulera och utföra vissa operationer på specifika delar av en sträng.

## Så här gör du:
I Gleam kan du använda funktionen `String.slice()` för att extrahera en del av en sträng. Här är ett exempel på hur man extraherar de första sex tecknen i en sträng:

```Gleam
let sträng = "Hej världen!"
let del = String.slice(sträng, 0, 6)
IO.println(del)
```
Detta kommer att skriva ut "Hej vä" eftersom det första argumentet är den ursprungliga strängen, det andra argumentet är startpositionen och det tredje argumentet är längden på den del som ska extraheras.

## Djupdykning:
Att extrahera substränger är en vanlig operation inom programmering och det finns många sätt att göra det. Alternativen inkluderar att använda inbyggda funktioner i andra programmeringsspråk som `substring` i Java och `substr` i JavaScript. Gleam erbjuder också funktionen `String.take()` och `String.drop()` som kan användas för att extrahera delar av en sträng baserat på antingen antal tecknen eller ett villkor.

Implementationen av `String.slice()` i Gleam är baserad på det funktionella programmeringsspråket Erlang, som Gleam bygger på. Detta ger en robust och effektiv implementation av substrängsextraktion i Gleam.

## Se även:
Här är några resurser för att lära dig mer om substrängsextraktion och hur det kan användas i Gleam:

- [Dokumentation för `String.slice()` i Gleam](https://gleam.run/documentation/std-lib-trip/0.1.1/String.html#function.slice)
- [En djupare titt på Erlang och dess påverkan på Gleams implementation av substrängsextraktion](https://www.researchgate.net/publication/310390220_String_Subtraction_Function_Using_Erlang)
- [En introduktion till funktionell programmering och dess fördelar i Gleam](https://gleam.run/articles/functional-programming.html)