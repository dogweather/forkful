---
date: 2024-01-20 17:55:54.764022-07:00
description: "Att l\xE4sa kommandoradsargument handlar om att extrahera anv\xE4ndarinput\
  \ direkt fr\xE5n terminalen. Programmerare g\xF6r detta f\xF6r att g\xF6ra sina\
  \ skript interaktiva\u2026"
lastmod: '2024-03-13T22:44:38.353687-06:00'
model: gpt-4-1106-preview
summary: "Att l\xE4sa kommandoradsargument handlar om att extrahera anv\xE4ndarinput\
  \ direkt fr\xE5n terminalen. Programmerare g\xF6r detta f\xF6r att g\xF6ra sina\
  \ skript interaktiva\u2026"
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

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
