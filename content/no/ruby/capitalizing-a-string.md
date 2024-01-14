---
title:                "Ruby: Stor bokstav på en streng"
simple_title:         "Stor bokstav på en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å ha en grundig forståelse av grunnleggende programmeringskonsepter er viktig for å kunne lage effektive og feilfrie koder. En av disse konseptene er å kunne endre størrelsen på tekststrenger på en enkel måte. Capitalizing, eller å gjøre første bokstav i et ord stor, er en vanlig oppgave i mange programmeringsprosjekter. Ved å lære dette enkle konseptet, kan du gjøre koden din mer lesbar og effektiv.

## Hvordan

Å endre størrelsen på en tekststreng i Ruby er veldig enkelt. Alt du trenger å gjøre er å bruke funksjonen '.capitalize', som gjør første bokstav i en tekststreng stor. Du kan også bruke funksjonen '.capitalize!' som endrer tekststrengens størrelse permanent.

La oss se på et eksempel:

```Ruby
tekststreng = "hei på deg"
puts tekststreng.capitalize
```
Output: "Hei på deg"

Du kan også gjøre dette med flere ord i en tekststreng:

```Ruby
tekststreng = "velkommen til ruby-programmering"
puts tekststreng.capitalize
```
Output: "Velkommen til ruby-programmering"

## Deep Dive

Å forstå hvordan '.capitalize' funksjonen fungerer, kan være nyttig når du har med brukergenerert input å gjøre. For eksempel, hvis du vil at den første bokstaven i brukerens navn skal være stor uansett hvordan de skriver det inn, kan du bruke denne funksjonen for å formatere det riktig.

Det er også viktig å huske på at størrelsen på tekststrenger er casesensitive. Dette betyr at funksjonen bare vil gjøre den første bokstaven stor og ikke forandre på andre bokstaver i strengen.

## Se også

- [Ruby string documentation](https://ruby-doc.org/core/String.html)
- [Ruby string methods](https://www.rubyguides.com/2018/10/ruby-string-methods/)
- [Ruby string casing](http://ruby-for-beginners.rubymonstas.org/objects/string.html#casing)