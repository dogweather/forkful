---
title:                "Generering av tilfeldige tall"
html_title:           "Ruby: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig del av mange programmeringsoppgaver, som simuleringer, spill og sikkerhetsprotokoller. Dette gjør det mulig å skape forskjellige situasjoner og resultater, noe som er nøkkelen til å lage dynamiske programmer.

## Slik gjør du det

For å generere tilfeldige tall i Ruby, kan du bruke følgende kode:

```Ruby
# Generer et tilfeldig heltall mellom 1 og 100
tilfeldig_tall = rand(1..100)

# Skriv ut resultatet
puts "Det tilfeldige tallet er: #{tilfeldig_tall}"
```

Dette vil produsere et tilfeldig tall hver gang koden kjøres. Du kan også generere tilfeldige desimaltall ved å bruke `rand()`-funksjonen uten noen argumenter. For å generere et tilfeldig tall innenfor et bestemt område, kan du bruke `rand()`-funksjonen med et område som angitt i følgende kode:

```Ruby
# Generer et tilfeldig tall mellom 50 og 100
tilfeldig_tall = 50 + rand(50)

# Skriv ut resultatet
puts "Det tilfeldige tallet er: #{tilfeldig_tall}"
```

## Dykk dypere

Når du bruker `rand()`-funksjonen i Ruby, er tallet som genereres basert på en "tilfeldig seed". En "seed" er et tall som setter i gang en tilfeldig prosess. Hvis du ikke angir en "seed", vil Ruby bruke tiden på datamaskinen din som "seed". Dette betyr at hvis koden kjøres på samme tid på forskjellige datamaskiner, vil den generere de samme tilfeldige tallene. Du kan også angi en "seed" for å sikre at du får den samme sekvensen av tilfeldige tall hver gang du kjører koden din. Dette er spesielt nyttig hvis du trenger å gjenskape en bestemt tilfeldig situasjon.

## Se også

- [Ruby random documentation](https://ruby-doc.org/core-3.0.0/Random.html)
- [Difference Between Ruby's rand() and srand() Methods](https://www.cresetter.com/blog/difference-between-rand-and-srand-in-ruby/)