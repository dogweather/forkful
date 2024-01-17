---
title:                "Sammanslagning av strängar"
html_title:           "Gleam: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Konkatenering av strängar är en vanlig uppgift inom programmering som innebär att man kombinerar flera strängar till en enda sträng. Detta kan vara användbart för att skapa dynamiska meddelanden eller att formatera data på ett önskat sätt.

## Hur man gör:
Det finns flera sätt att konkatenera strängar i Gleam, men det vanligaste sättet är att använda funktionen `String.concat`. Här är ett exempel på hur man kan använda den:
```Gleam
let name = "Jenny"
let message = String.concat(["Hej ", name])

``` 
Detta kommer att skapa en ny sträng som innehåller "Hej Jenny". Man kan även konkatenera mer än två strängar samtidigt genom att lägga till dem i en lista som parameter till `String.concat`.

## Djupdykning:
Konkatenering av strängar är en vanligt förekommande uppgift inom programmering och har funnits med sedan tidiga språk som C och Pascal. Det finns även andra sätt att konkatenera strängar i Gleam, som till exempel att använda operatorn `++` eller funktionen `lists.flat_map`, men det är upp till dig att bestämma vilket som passar bäst för ditt projekt.

För att göra processen med konkatenering av strängar mer effektiv använder sig Gleam av en datatyp som heter `String.Buffer`, som kan användas för att bygga upp en stor sträng bit för bit utan att behöva skapa en ny sträng varje gång.

## Se även:
- [Gleams officiella dokumentation för String modulen](https://gleam.run/docs/stdlib/string/)