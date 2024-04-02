---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:30.233986-07:00
description: "Een string kapitaliseren betekent het wijzigen van de eerste letter\
  \ van elk woord naar een hoofdletter. Programmeurs doen dit voor opmaak, consistentie\
  \ en\u2026"
lastmod: '2024-03-13T22:44:50.962414-06:00'
model: gpt-4-0125-preview
summary: "Een string kapitaliseren betekent het wijzigen van de eerste letter van\
  \ elk woord naar een hoofdletter. Programmeurs doen dit voor opmaak, consistentie\
  \ en\u2026"
title: Een string met hoofdletters maken
weight: 2
---

## Wat & Waarom?

Een string kapitaliseren betekent het wijzigen van de eerste letter van elk woord naar een hoofdletter. Programmeurs doen dit voor opmaak, consistentie en leesbaarheid, vooral in titels of koppen.

## Hoe te:

In Bash kun je strings op verschillende manieren kapitaliseren. Hier is een klassieke aanpak met `awk`:

```Bash
echo "hallo wereld" | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)} 1'
```

Uitvoer:
```
Hallo Wereld
```

Of, met puur Bash:

```Bash
string="hallo wereld"
capitalize() {
  echo "$1" | while IFS=" " read -r word; do 
    echo -n "${word^} " 
  done
  echo
}
capitalize "$string"
```

Uitvoer:
```
Hallo Wereld 
```

## Diepere Duik:

Vroeger was ‘awk’ het gereedschap bij uitstek voor tekstmanipulatie. Het is robuust maar minder intuïtief voor beginners. Met de evolutie van Bash, vooral vanaf versie 4, zijn mogelijkheden zoals stringmanipulatie verbeterd.

De `awk` methode is klassiek, het itereert door elk woord en maakt de eerste letter een hoofdletter. Puur Bash gebruikt parameteruitbreiding: `${word^}` maakt de eerste letter van `$word` een hoofdletter. Parameteruitbreiding is direct en snel, en vermindert de noodzaak van externe tools.

Waarom doet dit ertoe? Wel, strings kapitaliseren is een veelvoorkomende behoefte bij programmeertaken. Juiste kapitalisatie kan cruciaal zijn voor gebruikersinterfaces of gegevensverwerking waar de presentatie belangrijk is. Weten hoe je dit in je shell kunt doen, kan de dag redden.

## Zie Ook:

- Bash handleiding voor parameteruitbreiding: https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion
- `awk` introductie en gebruik: https://www.gnu.org/software/gawk/manual/gawk.html
- StackOverflow discussies over tekstmanipulatie in Bash: https://stackoverflow.com/questions/tagged/bash+string+capitalization
