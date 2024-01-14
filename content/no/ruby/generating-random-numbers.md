---
title:                "Ruby: Generering av tilfeldige tall"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være mange grunner til å generere tilfeldige tall i et Ruby-program. Kanskje du trenger å tilfeldig velge et element fra en liste, opprette et unikt passord eller simulere en tilfeldig hendelse. Uansett hva årsaken er, er det viktig å vite hvordan man kan gjøre dette i Ruby.

## Hvordan
Det finnes mange måter å generere tilfeldige tall i Ruby på, men vi skal se på de to mest vanlige metodene: `rand()` og `Random.new`.

```Ruby
# Generer et tilfeldig tall mellom 1 og 10
puts rand(1..10)

# Opprette et tilfeldig objekt av Random-klassen
random = Random.new
puts random.rand(100) # Generer et tilfeldig tall mellom 1 og 100
```

Kjøring av dette koden vil gi noe lignende dette som output:

```
7
42
```

## Deep Dive
Hvis du vil forstå mer om hvordan tilfeldige tall blir generert i Ruby, er det nyttig å vite at `rand()`-metoden egentlig bare er en forkortelse for `Random.new.rand()`. Dette betyr at alle parametrene til `rand()`-metoden egentlig blir sendt til `Random.new.rand()`. Dette gir deg muligheten til å spesifisere en egen startverdi for tilfeldighetsgenereringen ved å inkludere en `Seed`-parameter i `Random.new`-objektet. En seed-verdi sørger for at den samme sekvensen av tilfeldige tall blir generert hver gang programmet kjøres, noe som kan være nyttig for testing eller konsistente resultater.

## Se også
- [Rubys offisielle dokumentasjon om Random-klassen](https://ruby-doc.org/core-2.7.3/Random.html)
- [En Stack Overflow-tråd om hvordan seed-verdier fungerer i Ruby](https://stackoverflow.com/questions/39043187/ruby-rand-always-returns-same-value/39043539#39043539)
- [En guide om tilfeldighetsgenerering og seed-verdier i Ruby](https://www.rubyguides.com/2018/12/ruby-seed-random/)