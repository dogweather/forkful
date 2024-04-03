---
date: 2024-01-20 17:45:37.058976-07:00
description: "Hur man g\xF6r: ."
lastmod: '2024-03-13T22:44:38.324534-06:00'
model: gpt-4-1106-preview
summary: .
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## Hur man gör:
```Fish Shell
set full_string "Fisk skal är trevligt"
set start_index 6
set length 4

# Extrahera en substräng med start och längd
set substring (string sub -s $start_index -l $length -- $full_string)
echo $substring
# Output: skal
```

## Djupdykning
Att extrahera substrängar i Fish Shell är direkt och smidigt. Det har inte alltid varit så i shell-programmering, där beroende på verktyg kan syntaxen vara krånglig. Till skillnad från bash som använder parameterexpansion, erbjuder Fish inbyggda 'string'-kommandon som gör det hela mycket tydligare. Det finns förstås andra sätt att sköta detta på, till exempel med 'sed' eller 'awk', men de kan vara överkill för enkla ändamål och kräver oftast mer kod. När du använder Fish Shell's `string sub`-kommando, kan du enkelt ange startindex och längd för den substräng du vill ha.

## Se även:
- [Fish Shell Dokumentation för 'string'-kommandon](https://fishshell.com/docs/current/commands.html#string)
- [Fish Shell Tutorial om Stränghantering](https://fishshell.com/docs/current/tutorial.html#tut_strings)
- [Stack Overflow: Hur man extraherar substrängar i Fish Shell](https://stackoverflow.com/questions/tagged/fish)
