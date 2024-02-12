---
title:                "Een string omzetten naar kleine letters"
aliases: - /nl/bash/converting-a-string-to-lower-case.md
date:                  2024-01-28T21:57:35.409220-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het omzetten van strings naar kleine letters is het transformeren van alle alfabetische tekens in een string naar hun kleine lettervorm. Programmeurs zetten strings om naar kleine letters voor consistentie, vergelijking zonder hoofdlettergevoeligheid, en om te voldoen aan systeem- of applicatievereisten.

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
