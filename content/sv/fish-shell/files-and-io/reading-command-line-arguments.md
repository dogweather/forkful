---
date: 2024-01-20 17:55:54.764022-07:00
description: "How to: I historiens b\xF6rjan var terminalen det prim\xE4ra gr\xE4\
  nssnittet mellan anv\xE4ndare och dator. Fish Shell moderniserar detta koncept med\
  \ enkel syntax och\u2026"
lastmod: '2024-04-05T21:53:39.691752-06:00'
model: gpt-4-1106-preview
summary: "I historiens b\xF6rjan var terminalen det prim\xE4ra gr\xE4nssnittet mellan\
  \ anv\xE4ndare och dator."
title: "L\xE4sa in kommandoradsargument"
weight: 23
---

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
