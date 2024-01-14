---
title:                "Ruby: Søking og bytting av tekst"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen engasjere seg i å søke og erstatte tekst? Vel, det er en viktig del av programmering og kan være nyttig for å gjøre endringer i store tekstfiler, eller for å automatisere prosesser.

## Slik gjør du det

For å søke og erstatte tekst i Ruby, kan du bruke metoden `gsub`. La oss se på et eksempel:
```Ruby
original_tekst = "Hei, jeg heter Ruby og er et programmeringsspråk."
ny_tekst = original_tekst.gsub("Ruby", "Python")
puts ny_tekst
```
Dette vil gi følgende output:
```
Hei, jeg heter Python og er et programmeringsspråk.
```
Her ser du at vi har byttet ut ordet "Ruby" med "Python" i den opprinnelige teksten. Du kan også kombinere `gsub` metoden med regulære uttrykk for å søke og erstatte mer komplekse mønstre i teksten.

## Dypdykk

Det er viktig å være klar over at `gsub` metoden bare gjør endringer på kopier av tekststrengen, og ikke på den originale teksten. Dette er for å unngå uønskede endringer. Hvis du vil lagre endringene i den opprinnelige teksten, kan du bruke `gsub!` metoden, som vil endre teksten direkte.

Du kan også bruke `gsub` metoden for å erstatte tekst i flere filer samtidig. Dette kan være nyttig hvis du har en stor kodebase og trenger å gjøre endringer i flere filer på en gang.

## Se også

For mer informasjon om `gsub` metoden og søking og erstatning av tekst i Ruby, kan du se følgende ressurser:

- [Offisiell Ruby-dokumentasjon](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [RubyGuides: Searching and Replacing Text in Ruby](https://www.rubyguides.com/2019/03/ruby-gsub-method/)
- [RubySlang: gsub method](https://rubyslang.com/blog/gsub-method)