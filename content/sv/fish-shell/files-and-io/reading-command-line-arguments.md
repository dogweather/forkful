---
title:                "Läsa in kommandoradsargument"
aliases:
- /sv/fish-shell/reading-command-line-arguments.md
date:                  2024-01-20T17:55:54.764022-07:00
model:                 gpt-4-1106-preview
simple_title:         "Läsa in kommandoradsargument"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument handlar om att extrahera användarinput direkt från terminalen. Programmerare gör detta för att göra sina skript interaktiva och flexibla.

## How to:
```Fish Shell
# Argumenten tillgängliga via $argv
echo "Hej, $argv!"
```

Körning:
```bash
> fish greeting.fish Världen
Hej, Världen!
```

## Deep Dive
I historiens början var terminalen det primära gränssnittet mellan användare och dator. Fish Shell moderniserar detta koncept med enkel syntax och kraftfulla funktioner. Alternativ till `$argv` inkluderar att läsa från fil eller standard input. Implementationen bakom `$argv` är associerad med arrayer, där varje argument är en element i denna array, tillgänglig via dess index.

## See Also
- Fish dokumentation om arguments: [https://fishshell.com/docs/current/index.html#variables](https://fishshell.com/docs/current/index.html#variables)
- Tutorial om att skriva skript i Fish: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
