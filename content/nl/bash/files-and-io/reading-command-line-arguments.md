---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:54.073574-07:00
description: "Het lezen van commandoregelargumenten stelt scripts in staat zich anders\
  \ te gedragen op basis van gebruikersinvoer. Het is hoe scripts veelzijdig kunnen\u2026"
lastmod: '2024-03-13T22:44:50.999271-06:00'
model: gpt-4-0125-preview
summary: "Het lezen van commandoregelargumenten stelt scripts in staat zich anders\
  \ te gedragen op basis van gebruikersinvoer. Het is hoe scripts veelzijdig kunnen\u2026"
title: Commandoregelargumenten lezen
weight: 23
---

## Wat & Waarom?

Het lezen van commandoregelargumenten stelt scripts in staat zich anders te gedragen op basis van gebruikersinvoer. Het is hoe scripts veelzijdig kunnen zijn en niet slechts geschikt voor één specifieke taak.

## Hoe te:

```Bash
#!/bin/bash

# Print de naam van het script.
echo "Scriptnaam: $0"

# Print het eerste argument.
echo "Eerste argument: $1"

# Print alle argumenten.
echo "Alle argumenten: $@"
```

Voorbeelduitvoer ervan uitgaande dat je script 'example.sh' heet en je roept `./example.sh arg1 arg2` aan:

```
Scriptnaam: ./example.sh
Eerste argument: arg1
Alle argumenten: arg1 arg2
```

Door argumenten lussen:

```Bash
#!/bin/bash

# Lus door elk argument.
for arg in "$@"; do
  echo "Argument: $arg"
done
```

## Diepere Duik

Bash ondersteunt al eeuwen commandoregelargumenten; het zijn positionele parameters, `$0` tot `$9`, met `$@` en `$*` die alle tonen. `$0` is het script zelf, `$1` tot `$9` zijn het eerste tot en met het negende argument; haakjes zoals `${10}` zijn nodig vanaf het tiende argument.

Het gebruiken van `$@` is gewoonlijk beter dan `$*` omdat het correct omgaat met argumenten die spaties bevatten. `$@` geeft elk argument als een apart "woord"; `$*` combineert ze allemaal in één "woord".

Je kunt door argumenten heen schuiven met het commando `shift`, dat `$2` naar `$1` opschuift, en zo voorts, waarbij de oude `$1` wordt weggegooid.

Alternatieven? Zeker. `getopts` en `getopt` bieden meer controle voor opties (zoals -h voor hulp) en vlaggenparsen; bekijk ze als `$1`, `$2`,... niet voldoende zijn.

## Zie Ook

- Bash-handleiding over Speciale Parameters: https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html
- Gevorderde Bash-scriptgids: https://www.tldp.org/LDP/abs/html/
- `getopts` tutorial: https://wiki.bash-hackers.org/howto/getopts_tutorial
