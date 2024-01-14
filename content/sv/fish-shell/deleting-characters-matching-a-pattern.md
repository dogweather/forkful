---
title:                "Fish Shell: Radera tecken som matchar ett mönster"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Varför

Att ta bort tecken som matchar ett mönster kan vara användbart när man vill rensa en text eller fil från oönskat innehåll. Det kan också vara praktiskt när man vill ändra eller anpassa en text baserat på ett visst mönster.

## Hur man gör det

För att ta bort tecken som matchar ett mönster i Fish Shell kan man använda kommandot `string equal`. Detta kommando tar två parametrar: en variabel som innehåller textsträngen som ska modifieras, och ett mönster som beskriver vilka tecken som ska tas bort. Här är ett exempel på hur man kan använda detta kommando:

```Fish Shell

set text "Hej, det här är en text med onödiga tecken som ska tas bort."
set pattern "*ödiga tecken*"
string equal --regex -r text $pattern

```

Efter att detta kommando har utförts kommer variabeln `text` att ha reducerats till: "Hej, det här är en textom ska tas bort."

## Djupdykning

Mönstret som anges i `string equal` kommandot kan bestå av reguljära uttryck (regex), som ger en mer avancerad granskning av texten. Detta gör att man kan ta bort tecken som matchar ett visst mönster, oavsett platsen i texten. Det finns också olika flaggor som kan användas för att ändra hur matchningen utförs. Mer information om reguljära uttryck och flaggor finns i Fish Shells officiella dokumentation.

## Se också

- [Fish Shell dokumentation om string equal](https://fishshell.com/docs/current/cmds/string-equal.html)
- [Grundläggande regex för Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-regex)
- [Tutorial om reguljära uttryck för Shell-kommandon](https://www.shellscript.sh/regular-expressions.html#comparison) (på engelska)