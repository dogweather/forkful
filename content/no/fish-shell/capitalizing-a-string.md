---
title:                "Sette stor bokstav i en streng"
html_title:           "Fish Shell: Sette stor bokstav i en streng"
simple_title:         "Sette stor bokstav i en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Kapitalisering av tekststrenger i Fish Shell for Norske Programmerere

## Hva & Hvorfor?

Kapitalisering av en tekststreng innebærer å forvandle de små bokstavene i en streng til store bokstaver. Dette er viktig når programmerere ønsker å standardisere tekstdata eller gjøre sammenligninger av tekststrengene som er ikke følsomme for små og store bokstaver.

## Slik gjør du det:

I Fish Shell, kan du lettvint kapitalisere en tekststreng med 'string' kommandoen. Her er et eksempel:

```
Fish Shell
> set -l tekst "hej verden"
> string upper -q $tekst
```

Utgangen vil være:

```
HEJ VERDEN
```

## Dypdykk 

Historisk sett, har kapitalisering av tekststreng vært en viktig del av databehandlingsprosessen, spesielt når det gjelder datarensing og standardisering. Som alternativ i Fish kan du bruke 'tr' kommandoen som følger: 

```Fish Shell
> echo "hej verden" | tr '[:lower:]' '[:upper:]'
```

Dette vil også gi utgangen "HEJ VERDEN".

Både 'string' kommandoen og 'tr' fungerer på samme måte, ved å iterere over hver karakter i strengen og forvandle den til en stor bokstav hvis det er en liten bokstav. Valget mellom de to vil som regel være et spørsmål om personlig preferanse og behov.

## Se også:

1. Fish Shell's offisielle dokumentasjon på 'string' kommandoen: https://fishshell.com/docs/current/cmds/string.html
2. GNU 'tr' kommando: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html