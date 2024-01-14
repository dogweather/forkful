---
title:    "Ruby: Tilfeldig tallgenerering"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

# Hvorfor
Å generere tilfeldige tall er en viktig del av programmering og kan være nyttig for en rekke formål. Det kan bidra til å skape variasjon og realisme i spill, simuleringer og andre applikasjoner, eller til å tilfeldig velge elementer i en liste.

# Slik gjør du det
For å generere tilfeldige tall i Ruby, kan du bruke metoden `rand()`. Denne metoden tar en parameter, som bestemmer omfanget av tallene du ønsker å generere. For eksempel vil `rand(10)` generere et tilfeldig tall mellom 0 og 9.

```Ruby
puts rand(10)
# Output: 7
```

Du kan også gi en startverdi og en sluttverdi til `rand()`. Da vil metoden generere et tilfeldig tall innenfor dette omfanget. For eksempel, `rand(1..100)` vil generere et tall mellom 1 og 100.

```Ruby
puts rand(1..100)
# Output: 47
```

Hvis du ønsker å generere desimaltall, kan du bruke metoden `randf()`. Denne tar ikke noe parameter, og vil generere et tilfeldig desimaltall mellom 0 og 1.

```Ruby
puts randf()
# Output: 0.7486381695013785
```

# Dypdykk
Ruby bruker en pseudorandom algoritme for å generere tilfeldige tall. Dette betyr at tallene ikke er helt tilfeldige, men beregnet basert på en matematisk formel. For å sikre større tilfeldighet, kan du sette en såkalt "seed" for å endre startpunktet for algoritmen. Dette gjøres med metoden `srand()`, og du kan gi den et heltall som parameter.

```Ruby
srand(1234)
puts rand(10)
# Output: 2

srand(1234)
puts rand(10)
# Output: 2
```

Som du kan se, vil samme seed alltid generere det samme tilfeldige tallet. Dette kan være nyttig hvis du ønsker å teste eller sammenligne resultater.

# Se også
- [Ruby Dokumentasjon - Random Class](https://ruby-doc.org/core-2.6/Random.html)
- [Ruby Coding Challenges: Random Number Generator](https://www.rubyguides.com/2015/12/ruby-coding-challenges-random-numbers/)
- [How to Generate Random Numbers in Ruby](https://www.rubyguides.com/2015/08/ruby-random/)