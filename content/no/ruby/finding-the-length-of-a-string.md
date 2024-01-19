---
title:                "Finne lengden på en streng"
html_title:           "Go: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvordan finne lengden på en streng i Ruby 

## Hva & Hvorfor?
Å finne lengden på en streng handler om å telle antallet tegn i den gitte teksten, inkludert mellomrom og spesialtegn. Dette er nyttig for mange formål, som å validere innskrift, begrense tekstlengde, eller generere dynamisk innhold basert på strenglengden.

## Hvordan:
For å finne lengden på en streng i Ruby, bruker vi `.length` eller `.size` metode.

```Ruby
tekst = "God dag, Norge!"
puts tekst.length  # Skriver ut: 15
puts tekst.size    # Skriver ut: 15
```
Begge vil gi samme resultat. Du kan bruke den som føles mest naturlig for deg.

## Dyp Dykk:
Historisk sett har `.length` og `.size` eksistert side om side i Ruby siden språkets begynnelse. Dette skyldes i stor grad Rubys influanser fra andre språk som Perl og Smalltalk, som brukte forskjellige termer for å referere til det samme konseptet.

Som et alternativ til `.length` eller `.size`, kan du også bruke `.count`, men vær oppmerksom på at `.count` fungerer litt annerledes. Det teller antallet forekomster av et visst tegn (eller tegn) i strengen, ikke strengens totale lengde.

```Ruby
tekst = "God dag, Norge!"
puts tekst.count('g')  # Skriver ut: 2
```
Over eksempel viser at 'g' forekommer 2 ganger i strengen.

Når det kommer til implementeringsdetaljer, er `.length` og `.size` stort sett synonyme i Ruby. De er implementert på samme måte og gir identisk ytelse, så valget mellom dem er stort sett et spørsmål om personlig preferanse.

## Se også:
For mer informasjon, sjekk ut disse ressursene:

1. Ruby API-dokumentasjon for String: [https://ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
2. StackOverflow-tråden om ".length vs .size": [https://stackoverflow.com/questions/6083219/length-vs-size-in-ruby](https://stackoverflow.com/questions/6083219/length-vs-size-in-ruby)
3. RubyMonk øvelser om Strings: [https://rubymonk.com/learning/books/1-ruby-primer/chapters/5-strings/lessons/31-string-basics]
(https://rubymonk.com/learning/books/1-ruby-primer/chapters/5-strings/lessons/31-string-basics)