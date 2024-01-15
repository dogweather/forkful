---
title:                "Store bokstaver i en streng"
html_title:           "Ruby: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å endre en streng, eller tekst, til store bokstaver kan være nyttig for å gjøre den mer leselig eller for å følge standard konvensjoner for tekstformatering.

## Slik gjør du det

For å gjøre dette i Ruby, kan du bruke metoden `upcase` for å konvertere en streng til store bokstaver. Her er et eksempel:

```ruby
tekst = "dette er en test"
puts tekst.upcase
```
Dette vil gi følgende utput:

```ruby
DETTE ER EN TEST
```

## Dypdykk

I Ruby finnes det også en annen metode for å endre en streng til store bokstaver kalt `capitalize`. Denne metoden vil bytte ut den første bokstaven i strengen med en stor bokstav og gjøre resten av teksten til små bokstaver. Se eksemplet under:

```ruby
tekst = "en annen test"
puts tekst.capitalize
```
Dette vil gi følgende utput:

```ruby
En annen test
```

Det finnes også metoder for å bare endre første bokstaven i en streng til stor bokstav (`capitalize!`) eller for å gjøre hele strengen til små bokstaver (`downcase`). Å bruke disse metodene kan være nyttig for å formatere tekst på en konsistent måte i koden din.

## Se også

- [Ruby dokumentasjon for string metoder](https://ruby-doc.org/core-2.7.1/String.html)