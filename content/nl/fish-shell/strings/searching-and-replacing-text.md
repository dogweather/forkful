---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:59.744270-07:00
description: "Tekst zoeken en vervangen is het vinden van specifieke reeksen en deze\
  \ omwisselen met iets anders. Programmeurs doen dit om code bij te werken, fouten\
  \ te\u2026"
lastmod: '2024-03-13T22:44:51.230069-06:00'
model: gpt-4-0125-preview
summary: Tekst zoeken en vervangen is het vinden van specifieke reeksen en deze omwisselen
  met iets anders.
title: Zoeken en vervangen van tekst
weight: 10
---

## Wat & Waarom?
Tekst zoeken en vervangen is het vinden van specifieke reeksen en deze omwisselen met iets anders. Programmeurs doen dit om code bij te werken, fouten te corrigeren, of om gegevens te herformatteren — het is een enorme tijdsbesparing.

## Hoe te:
Laten we alle instanties van 'kat' naar 'hond' veranderen in een string.

```Fish Shell
echo "Een kat, twee katten, drie katten." | string vervang -a 'kat' 'hond'
```
Voorbeelduitvoer:
```
Een hond, twee honden, drie honden.
```
Tekst in een bestand genaamd `pets.txt` vervangen:

```Fish Shell
string vervang -a 'kat' 'hond' < pets.txt > updated_pets.txt
```

Variabelen gebruiken voor patronen:

```Fish Shell
set oud "kat"
set nieuw "hond"
string vervang -a $oud $nieuw < pets.txt > updated_pets.txt
```

## Diepere duik
Zoeken en vervangen bestaat al sinds de vroege dagen in teksteditors. Denk aan `sed` voor streambewerking in Unix — dat is ouderwets cool. Fish neemt dit verder en maakt het eenvoudiger met het `string` commando. Geen regex-hoofdpijn meer, tenzij je dat wilt. Alternatieven? Zeker: `sed`, `awk`, Perl-scripts, zelfs `vim`-macro's. Maar het `string` commando van Fish is elegant en minder foutgevoelig voor de gangbare gevallen.

## Zie ook:
- Officiële documentatie van Fish Shell over het `string` commando: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Sed by Example, Deel 1: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- AWK Language Programming — Stringfuncties: [https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions](https://www.gnu.org/software/gawk/manual/gawk.html#String-Functions)
