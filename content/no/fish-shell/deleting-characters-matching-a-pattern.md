---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Sletting av karakterer som samsvarer med et mønster (pattern matching) er en metode for å filtrere ut spesifikke deler fra en tekst. Det brukes ofte i programmering for å manipulere data, for eksempel for å manipulere tekststrenger, rense data eller hente ut informasjon.

## Hvordan gjøre det:

For å slette en karakter fra en streng med custonm ops match, kan du bruke `string`-kommandoen i Fish Shell som følger:

```Fish Shell
set tekst "Hello, world!"
string replace -r -a ',' '' $tekst
```

Utførelsen av koden gir oss følgende resultat:

```Fish Shell
Hello world!
```

## Dyp Dykking

Funksjonaliteten for pattern matching har vært en del av Shell-skript og Unix-verktøy i flere tiår. I Fish, er `string`-kommandoen brukt i stedet for det eldre `sed` eller `awk`-verktøyene for å gi en enklere og mer intuitiv opplevelse.

Et alternativ til å bruke `string`-kommandoen for å slette karakterer, er å bruke `awk` eller `sed`-verktøyene for mer komplekse operasjoner. Samtidig er forskjellige programmeringsspråk også utstyrt med lignende funksjoner for patern matching.

Når det gjelder implementeringen i Fish, har `string replace`-kommandoen flere alternativer for styring av hvilke strenger som skal erstattes, for eksempel `-r` for å bruke regular expressions, og `-a` for å erstatte alle forekomster.

## Se også:
1. Fish Documentation: [www.fishshell.com/docs/current](www.fishshell.com/docs/current)
2. String handling: [wiki.bash-hackers.org/syntax/pe](wiki.bash-hackers.org/syntax/pe)
3. Unix for Poets: [https://www.cs.upc.edu/~padro/Unixforpoets.pdf](https://www.cs.upc.edu/~padro/Unixforpoets.pdf)