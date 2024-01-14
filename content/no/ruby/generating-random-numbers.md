---
title:                "Ruby: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor

Programmering er et allsidig og kreativt verktøy som kan brukes til å løse en rekke problemer og utforske ulike konsepter. En av de hyppig brukte funksjonene i programmering er å generere tilfeldige tall. Dette kan være nyttig for å simulere situasjoner, sikre sikkerhet og for å skrive underholdende spill. I denne bloggposten skal vi utforske hvordan man kan generere tilfeldige tall ved hjelp av Ruby-programmeringsspråket.

# Hvordan

For å generere tilfeldige tall i Ruby, kan vi bruke metoden `rand` som tar inn et argument for det maksimale tallet vi ønsker å generere. La oss for eksempel si at vi ønsker å generere et tilfeldig tall mellom 1 og 10. Da kan vi bruke følgende kode:

```ruby
tilfeldig_tall = rand(10)
puts tilfeldig_tall
```

Når vi kjører dette eksempelet, vil programmet generere et tilfeldig tall mellom 0 og 9 og skrive det ut til konsollen. Dette er fordi `rand`-metoden inkluderer 0 som et mulig tall. Hvis vi ønsker å generere et tall mellom 1 og 10, kan vi legge til 1 i `rand`-metodens argument:

```ruby
tilfeldig_tall = rand(10) + 1
puts tilfeldig_tall
```

I tillegg til å generere tilfeldige heltall, kan vi også generere tilfeldige desimaltall ved å bruke `rand`-metoden på følgende måte:

```ruby
tilfeldig_desimaltall = rand
puts tilfeldig_desimaltall
```

Dette vil generere et tilfeldig desimaltall mellom 0 og 1 og skrive det ut til konsollen.

# Dypdykk

Nå som vi har sett på hvordan vi kan generere tilfeldige tall i Ruby, la oss se på hvordan dette fungerer i bakgrunnen. Ruby bruker en algoritme kalt "Mersenne Twister" for å generere tilfeldige tall. Denne algoritmen ble utviklet av Makoto Matsumoto og Takuji Nishimura i 1997 og er en av de mest brukte og anerkjente algoritmene for å generere tilfeldige tall.

En av de viktigste egenskapene til Mersenne Twister-algoritmen er at den produserer en sekvens med tall som har en veldig lang periode før de begynner å gjenta seg. Dette gjør den til et pålitelig og effektivt verktøy for å generere tilfeldige tall i programmeringssammenheng.

# Se også

- [Ruby offisiell dokumentasjon om rand-metoden](https://ruby-doc.org/core-2.7.1/Random.html#method-i-rand)
- [Algorithms and Randomness, en introduksjon til tilfeldige tall og algoritmer](https://towardsdatascience.com/algorithms-and-randomness-47b425f92f1c)
- [The Mersenne Twister random number generator](https://en.wikipedia.org/wiki/Mersenne_Twister) (engelsk)