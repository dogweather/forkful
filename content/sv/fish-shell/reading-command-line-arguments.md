---
title:                "Läsning av kommandoradsargument"
html_title:           "Fish Shell: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att läsa kommandoradsargument är när du tar in information som användaren matar in direkt från kommandoraden, istället för att hardkoda den i ditt program. Detta är användbart eftersom det ger användaren mer kontroll och flexibilitet över hur programmet körs.

## Så här:

```
Fish Shell har inbyggda kommandon och variabler för att läsa in kommandoradsargument.

# ~ `argv` är en variabel som innehåller en lista över alla argument som matas in.
$ fish my_program.fish hello world

# Output:
$ argv = ('hello', 'world')

# Du kan också använda `count`-kommandot för att räkna antalet argument.
$ fish my_program.fish one two three

# Output:
$ count $argv
3

# Om du vill läsa ett specifikt argument från listan, kan du använda `set`-kommandot.
$ fish my_program.fish hello world

# Output:
$ set argument $argv[1]
hello
```

## Deep Dive:

Att läsa kommandoradsargument är en vanlig praxis inom programmering och stöds av många olika programmeringsspråk och skalor. Alternativ till Fish Shell inkluderar bash, zsh och PowerShell.

När du matar in kommandon och argument i Fish Shell, sparar den dem som en lista i variabeln `argv` och numreras från 1. Det är bra att notera att kommandot självt kommer att listas som det första argumentet.

## Se även:

- Information om Fish Shell på deras hemsida: https://fishshell.com
- Artikel om Fish Shell på Wikipedia: https://sv.wikipedia.org/wiki/Fish_(programmeringsspråk)
- Dokumentation om hur man läser argument i andra programmeringsspråk såsom Python och Java.