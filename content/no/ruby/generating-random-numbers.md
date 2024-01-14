---
title:    "Ruby: Å generere tilfeldige tall"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere er kjent med betydningen av å kunne generere tilfeldige tall i sitt arbeid. Enten det er for å lage spill, simulere virkelige situasjoner eller lage sikre passord, blir generering av tilfeldige tall en viktig del av utviklingen. I denne artikkelen vil vi diskutere hvorfor å kunne generere tilfeldige tall er viktig og hvordan man kan gjøre det i Ruby.

## Hvordan gjøre det

I Ruby kan vi enkelt generere tilfeldige tall ved hjelp av Random-klassen. Vi kan bruke `rand` metoden og angi en grense for hvor stor vi vil at det tilfeldige tallet skal være. For eksempel:

```Ruby
puts rand(10)
```

Dette vil generere et tilfeldig tall mellom 0 og 10. Hvis vi ønsker å få et heltall i et bestemt område, kan vi bruke `rand` metoden sammen med `.to_i` for å konvertere til et heltall. For eksempel:

```Ruby
puts rand(1..100).to_i
```

Dette vil generere et tilfeldig heltall mellom 1 og 100. Området kan også være en liste av tall, for eksempel `rand([1,3,5])`, som vil generere enten 1, 3 eller 5 som et tilfeldig tall.

I tillegg til `rand` metoden, har Ruby også en `srand` metode som brukes til å sette en startverdi for den tilfeldige tallgeneratoren. Hvis vi ønsker at tallgeneratoren skal generere de samme tallene hver gang vi kjører programmet, kan vi bruke denne metoden. For eksempel:

```Ruby
srand 123
puts rand(10) # vil alltid generere 2
puts rand(10) # vil alltid generere 9
```

## Dypdykk

I Ruby bruker `rand` og `srand` metoden en pseudorandom algoritme for å generere tall. Dette betyr at tallene ikke er helt tilfeldige, men følger et bestemt mønster. Denne algoritmen bruker en startverdi og genererer en sekvens med tall basert på denne startverdien. Om vi bruker samme startverdi vil vi alltid få den samme sekvensen med tall.

Det finnes også andre metoder for å generere tilfeldige tall i Ruby, for eksempel `secure_random` biblioteket som bruker en mer avansert tilfeldighetsalgoritme. Dette er spesielt viktig hvis man skal generere sikre passord eller krypteringsnøkler.

## Se også

- [Ruby dokumentasjon for Random klassen](https://ruby-doc.org/core-2.7.1/Random.html)
- [Artikkel om tilfeldige tall i Ruby](https://www.tutorialspoint.com/ruby/ruby_numbers.htm)