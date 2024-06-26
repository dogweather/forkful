---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:35.409220-07:00
description: 'Hoe: Hier is de eenvoudige manier om een string naar kleine letters
  om te zetten in Bash.'
lastmod: '2024-03-13T22:44:50.966591-06:00'
model: gpt-4-0125-preview
summary: Hier is de eenvoudige manier om een string naar kleine letters om te zetten
  in Bash.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe:
Hier is de eenvoudige manier om een string naar kleine letters om te zetten in Bash:

```Bash
str="Make Me Lower Case"
lower_str=$(echo "$str" | tr '[:upper:]' '[:lower:]')

echo $lower_str
```

Uitvoer:

```
make me lower case
```

Bash 4.0 en hoger heeft een ingebouwde methode met parameteruitbreiding:

```Bash
str="Make Me Lower Case"
lower_str="${str,,}"

echo $lower_str
```

Uitvoer:

```
make me lower case
```

## Diepgaande Duik
Voor Bash 4.0 werden vaak gebruikte methoden om strings naar kleine letters om te zetten, uitgevoerd met externe hulpprogramma's zoals `tr`, `awk` of `sed`. Elk van deze biedt verschillende manieren om strings te manipuleren, meer dan alleen het wijzigen van de case, maar kan het nodig zijn om een nieuw proces te starten, wat de prestaties beïnvloedt.

De introductie van de `${parameter,,pattern}` syntaxis in Bash 4.0 bood een native functie om strings te transformeren, die sneller is en niet afhankelijk is van externe hulpprogramma's. Er zijn alternatieven binnen Bash zelf:

1. `awk`: `echo $str | awk '{print tolower($0)}'`
2. `sed`: `echo $str | sed 's/[A-Z]/\L&/g'`
3. `tr`: `echo $str | tr '[:upper:]' '[:lower:]'` - zoals hierboven getoond.

Wat betreft de implementatie, `${parameter,,pattern}` verandert niet alleen ASCII-tekens; ze zijn zich bewust van UTF-8 en kunnen niet-Engelse tekens aan, waardoor ze veelzijdig zijn voor internationale toepassingen.

## Zie Ook
- Bash Parameteruitbreiding: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- `tr` Commando: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- AWK Programmering: https://www.gnu.org/software/gawk/manual/gawk.html
- `sed` Stream Editor: https://www.gnu.org/software/sed/manual/sed.html
