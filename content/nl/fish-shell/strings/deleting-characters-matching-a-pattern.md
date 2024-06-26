---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:29.383301-07:00
description: "Hoe te: In Fish Shell gebeurt de magie met de `string` utility, een\
  \ handige ingebouwde tool voor stringbewerkingen - ge\xEFntroduceerd in versie 2.3.0.\u2026"
lastmod: '2024-04-05T21:53:51.236832-06:00'
model: gpt-4-0125-preview
summary: "In Fish Shell gebeurt de magie met de `string` utility, een handige ingebouwde\
  \ tool voor stringbewerkingen - ge\xEFntroduceerd in versie 2.3.0."
title: Karakters verwijderen die overeenkomen met een patroon
weight: 5
---

## Hoe te:
```Fish Shell
# Cijfers uit een string verwijderen
set string "Fish123Shell"
echo $string | string vervang -ra '[0-9]' ''
# Uitvoer: FishShell

# Alles behalve kleine letters eruit halen
set lawaaierige_string "F!i@s#h$%S^h&e*l(l)__+"
echo $lawaaierige_string | string overeenkomst -r '[a-z]+'
# Uitvoer: ishhell
```

## Diepgaand Onderzoek
In Fish Shell gebeurt de magie met de `string` utility, een handige ingebouwde tool voor stringbewerkingen - geïntroduceerd in versie 2.3.0. Voorheen vielen gebruikers terug op UNIX-standaarden zoals `sed` of `awk`. Waarom de verandering? Eenvoud en integratie. Het hebben van een in-huis oplossing stroomlijnt stringmanipulatie, waardoor scripts leesbaarder en onderhoudsvriendelijker worden.

Alternatieven? Zeker, de oude garde `sed` kan nog steeds het werk doen:

```Fish Shell
set ouderwetse_string "Fish@Shell2023"
echo $ouderwetse_string | sed 's/[0-9]//g'
# Uitvoer: Fish@Shell
```

Maar waarom niet de eigen tools van Fish benutten? Voor implementatie heeft `string vervang` een `-r` optie die regexpatronen mogelijk maakt. `-a` past het commando toe op alle overeenkomsten, en het toevoegen van een '' aan het eind vertelt het om te vervangen met niets, oftewel, te verwijderen. Gebruik `string overeenkomst` wanneer je zoekt naar een patroon om te behouden, in plaats van wat weg te gooien.

## Zie Ook
- Officiële Fish Shell Documentatie over `string`: https://fishshell.com/docs/current/cmds/string.html
- Regex tutorial voor diepgaand onderzoek naar patronen: https://www.regular-expressions.info/
- Sed & Awk, oude tekstkrachten: een introductie: https://www.gnu.org/software/sed/manual/sed.html, http://www.grymoire.com/Unix/Awk.html
