---
title:    "Gleam: Att använda reguljära uttryck"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Att använda reguljära uttryck (regular expressions) kan göra programmering mer effektivt och automatiserat. Genom att använda reguljära uttryck kan du enkelt hitta och manipulera text baserat på mönster, istället för att manuellt gå igenom långa strängar av text.

## Hur man gör

För att använda reguljära uttryck i Gleam, behöver du först importera Regex modulen. Sedan kan du använda funktioner som `match` och `replace` för att söka efter och manipulera text baserat på ett visst mönster. Här är ett enkelt exempel på hur man skulle kunna använda reguljära uttryck för att hitta alla ord som börjar med stor bokstav i en textsträng:

```
Gleam import Regex

let text = "Detta är en textsträng med Ord."

let pattern = Regex.regex("[A-Z][a-z]+")

let words = Regex.match(pattern, text)

words |> IO.print // Output: [Ord]
```

## Fördjupning

Reguljära uttryck är ett kraftfullt verktyg som kan användas för att söka, matcha och manipulera text baserat på mönster. I Gleam kan du använda metakaraktärer som `+`, `*`, `?` och `|` för att skapa mer komplexa mönster. Du kan också använda grupperingar med parenteser och referera till dem i dina ersättningssträngar med hjälp av `\1`, `\2` osv.

En annan användbar funktion i Regex-modulen är `split`, som låter dig dela upp en textsträng baserat på ett visst mönster istället för ett statiskt tecken. Detta kan vara användbart när du behöver behandla text som inte följer en strikt mall.

## Se också

- [Gleam Regex-modulen](https://gleam.run/modules/regex/)
- [Reguljära uttryck cheat sheet](https://www.rexegg.com/regex-quickstart.html)
- [En interaktiv tutorial för reguljära uttryck](https://regexone.com/)