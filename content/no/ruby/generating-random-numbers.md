---
title:                "Genererer tilfeldige tall"
html_title:           "PHP: Genererer tilfeldige tall"
simple_title:         "Genererer tilfeldige tall"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall handler om å produsere tall innenfor en viss rekkevidde som ikke er forutsigbare. Programmerere gjør dette for å sikre variasjon i beregninger og simuleringer, og for kryptografi og sikkerhetsformål.

## Slik gjør du det:


```Ruby
# For å generere et tilfeldig tall mellom 0 og 1
tilfeldig = rand()              
puts tilfeldig                   

# For å generere et tilfeldig tall mellom 0 og 100
tilfeldig = rand(100)           
puts tilfeldig                   

# For å generere et tilfeldig tall mellom 20 og 30
tilfeldig = rand(20..30)       
puts tilfeldig                   
```
Hvis du kjører ovenstående kode, vil du se forskjellige tall hver gang fordi tallene er tilfeldig generert.

## Dypdykk
Historisk sett, før datamaskinene, brukte vi mekaniske enheter som terninger, mynter og rouletthjul for å generere tilfeldige tall. Med adventen til datamaskiner og programmeringsspråk som Ruby, kan vi generere tilfeldige tall elektronisk.

Alternativene til `rand` -metoden i Ruby inkluderer metoder fra Gem `Faker` som lar deg generere tilfeldige nummerdata innenfor bestemte kriterier og mengder.

Implementeringen av tilfeldige tall i Ruby bruker en metode kjent som Mersenne Twister, en pseudorandom number generator (PRNG). Det er en 'pseudorandom' fordi tallene generert er uforutsigbare under normale forhold, men hvis du kjenner algoritmen og tilstanden den er i, kan du forutsi tallene.

## Se også
1. Stabling av seddel: [Ruby-dokumentasjon på Random Numbers](https://ruby-doc.org/core-2.5.1/Random.html)