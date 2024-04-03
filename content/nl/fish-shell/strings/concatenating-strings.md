---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:42.154829-07:00
description: 'Hoe: In Fish plakt u strings aan elkaar met spaties ertussen of gebruikt
  u `string join`.'
lastmod: '2024-03-13T22:44:51.237060-06:00'
model: gpt-4-0125-preview
summary: In Fish plakt u strings aan elkaar met spaties ertussen of gebruikt u `string
  join`.
title: Samenvoegen van strings
weight: 3
---

## Hoe:
In Fish plakt u strings aan elkaar met spaties ertussen of gebruikt u `string join`.

```fish
# Combineer 'Hello' en 'World!' met een spatie
echo 'Hello' 'World!'

# Uitvoer: Hello World!

# Variabelen aaneenschakelen
set greet "Howdy"
set who "Partner"
echo $greet $who

# Uitvoer: Howdy Partner

# Aaneenschakeling zonder spaties met string join
set file "report"
set ext "txt"
string join '' $file '.' $ext

# Uitvoer: report.txt
```

## Diepere Duik
Aaneenschakeling bestaat al sinds de dageraad van het programmeren. In Fish is `string join` schoner dan oudere methoden, zoals het gebruik van `echo` gevolgd door string-variabelen zonder aanhalingstekens. Deze benadering vermijdt de overhead van subcommando's, wat een prestatievoordeel kan zijn.

Alternatieven zijn onder andere het gebruik van `printf`, wat meer controle over de opmaak biedt maar net iets complexer is voor eenvoudige aaneenschakelingsoperaties. Voorbeeld:

```fish
set firstName "Ada"
set lastName "Lovelace"
printf "%s %s\n" $firstName $lastName
```

Fish's `string` commando maakt deel uit van een ingebouwde gereedschapskist voor het manipuleren van strings, geïntroduceerd om tekstverwerking eenvoudiger te maken. Het is niet uniek voor Fish, maar zijn opname als een ingebouwd hulpmiddel houdt zaken eenvoudig.

## Zie Ook
- Officiële Fish-documentatie: [link](https://fishshell.com/docs/current/cmds/string.html)
- Community-tutorials: [link](https://fishshell.com/docs/current/tutorial.html#tutorial)
- Discussie over stringmanipulatie in shells: [link](https://unix.stackexchange.com/questions/131766/why-does-my-shell-script-choke-on-whitespace-or-other-special-characters)
