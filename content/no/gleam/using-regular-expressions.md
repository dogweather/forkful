---
title:                "Å bruke regulære uttrykk"
html_title:           "Gleam: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regular Expressions, eller bare regex, er en måte å definere og søke etter mønstre i tekst på. Dette er spesielt nyttig for programmerere som ønsker å finne og manipulere data på en effektiv måte.

## Hvordan:
For å bruke regex i Gleam, må vi importere modulen `gleam/re/regex` og bruke funksjonen `match` sammen med et mønster og en tekststreng. La oss si at vi vil finne alle tall i en tekststreng og sjekke om de alle er partall. Da kan vi skrive følgende:
```Gleam
text =
"Even numbers: 2, 4, 6, 8"
pattern = "\\d"
match(text, pattern)
```
Dette vil gi oss en liste med matchende tall (2,4,6,8), som vi så kan filtrere gjennom og sjekke om de er partall eller ikke.

## Dypp ned:
Regex har eksistert siden 1950-tallet og er en standard i mange programmeringsspråk. Gleam bruker biblioteket Oniguruma for å implementere regex, som støtter et bredt utvalg av mønstre og har lang historie med å være et pålitelig verktøy for å manipulere tekst.

En alternativ metode for å finne og manipulere tekst på en lignende måte er å bruke string-metoder som `index_of` og `replace`, men disse metodene vil kunne bli langtekkelige og mer kompliserte for komplekse mønstre.

For å mestre regex er det viktig å bli kjent med de ulike symbolene og syntaxen som brukes for å uttrykke mønstre. Det finnes også mange ressurser på nettet, inkludert dokumentasjon og verktøy som kan hjelpe deg å teste og debugge dine regex uttrykk.

## Se også:
For mer informasjon om regex i Gleam, kan du sjekke dokumentasjonen på nettstedet: https://gleam.run/lib/regex. Det finnes også mange ressurser på nettet som tilbyr interaktive øvelser og tutorials for å hjelpe deg å bli mer komfortabel med å bruke regex. Sjekk ut "RegexOne" og "Regexr" for å komme i gang. Lykke til med å manipulere tekst på en rask og effektiv måte med regex i Gleam!