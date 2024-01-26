---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:50:03.732384-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Generering av tilfeldige tall er å skape et tall som ikke kan forutsies logisk. Programmerere gjør dette for spill, sikkerhet, simuleringer og steder hvor uforutsigbarhet gir verdi. 

## Hvordan:
Her er eksempler på hvordan du genererer tilfeldige tall i Ruby. 

```Ruby
# Enkel tilfeldig heltall generering
tilfeldig_tall = rand(10) # Gir et tall mellom 0 og 9
puts tilfeldig_tall

# Tilfeldig float mellom 0 og 1
tilfeldig_float = rand
puts tilfeldig_float

# Tilfeldig tall i et intervall
tilfeldig_range_tall = rand(5..10) # Gir et tall mellom 5 og 10
puts tilfeldig_range_tall
```

Eksempel på kjøring:

```
7
0.4376284928336724
9
```

## Dypdykk
Generering av tilfeldige tall er ikke egentlig "tilfeldig" i informatikk; det kalles pseudotilfeldigheter. Historisk sett har metoder variert fra fysiske enheter til matematiske algoritmer. I Ruby, bruker vi `rand`-metoden tilbudt av `Random` klassen, som implementerer en Mersenne Twister-generator, kjent for sin hurtighet og lange periode.

I stedet for `rand`, kan du også bruke `SecureRandom` biblioteket for kryptografisk sikre tilfeldige tall, noe som er viktig for sikkerhetsrelaterte oppgaver.

```Ruby
require 'securerandom'

# Tilfeldig hex-verdi
tilfeldig_hex = SecureRandom.hex(10)
puts tilfeldig_hex

# Tilfeldig base64-verdi
tilfeldig_base64 = SecureRandom.base64(10)
puts tilfeldig_base64
```

## Se Også
- [Ruby-Dokumentasjon: Random Class](https://ruby-doc.org/core-3.1.0/Random.html)
- [Ruby-Dokumentasjon: SecureRandom](https://ruby-doc.org/stdlib-3.1.0/libdoc/securerandom/rdoc/SecureRandom.html)
- [Ruby-Dokumentasjon: Kernel#rand](https://ruby-doc.org/core-3.1.0/Kernel.html#method-i-rand)
- [Artikkel om pseudotilfeldighetsgeneratorer](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
