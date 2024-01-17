---
title:                "Skrivande till standardfel"
html_title:           "Fish Shell: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att skriva till standarderror, även kallad stderr, är en vanlig praxis inom programmering. Det är ett sätt att skicka felmeddelanden och varningar till användaren istället för att bara skriva ut dem i terminalen. Detta gör det enklare att identifiera och åtgärda eventuella fel i koden.

## Hur man gör:

```Fish Shell``` har ett inbyggt kommando, "echo", som används för att skriva text till standardoutput. För att skriva till standarderror, behöver vi bara lägga till flaggan "-e" och riktningen "&2" efter kommandot, som i exemplet nedan:

```Fish Shell
echo -e "Det här är ett felmeddelande" >&2
```

Output: Det här är ett felmeddelande

## Djupdykning:

Att skriva till standarderror används ofta tillsammans med felhantering i programmeringsspråk. Istället för att bara skriva ut felmeddelanden i terminalen, kan programmet skriva dem till standarderror för att separera dem från vanliga utskrifter.

Det finns också andra sätt att skriva till standarderror, som att använda "printf" istället för "echo". Det är viktigt att lägga märke till att standarderror inte alltid visas i terminalen, utan kan skickas till en annan output, som en loggfil.

## Se även:

- https://fishshell.com/docs/current/tutorial.html#tut_error
- https://linux.die.net/man/1/fish