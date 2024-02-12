---
title:                "Refaktorering"
aliases:
- /no/ruby/refactoring/
date:                  2024-01-26T03:36:51.439399-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Omstrukturering er prosessen med å restrukturere eksisterende dataprogramkode uten å endre dens eksterne oppførsel. Programmerere omstrukturerer for å forbedre de ikke-funksjonelle attributtene til programvaren, som lesbarhet, redusert kompleksitet, forbedret vedlikeholdbarhet, eller ytelsesforbedring.

## Hvordan:

La oss gå gjennom et eksempel på omstrukturering av en Ruby-metode som beregner summen av kvadrater.

**Før omstrukturering:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # Utdata: 14
```

**Etter omstrukturering:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # Utdata: 14
```

Den omstrukturerte versjonen bruker Ruby Enumerables for å uttrykke den samme logikken mer kortfattet og klart. `map`-metoden transformerer hvert element, og `sum` aggregerer deres verdier, og fjerner behovet for manuell loop-håndtering og variabeltildeling.

## Dypdykk

Omstrukturering har en rik historisk kontekst, som strekker seg tilbake til tidlige praksiser i programvareutviklingen. Første omtaler kan spores tilbake til 1990-tallet, med betydelige bidrag fra Martin Fowler i boken hans "Refactoring: Improving the Design of Existing Code", hvor han gir et katalog over mønstre for omstrukturering. Siden da har omstrukturering blitt en hjørnestein i smidige utviklingspraksiser.

Når vi snakker om alternativer til omstrukturering, må vi enten vurdere en annen tilnærming som 'Omskriving', der du erstatter det gamle systemet delvis eller helt, eller tilpasse praksiser som 'Kodegjennomgang' og 'Parprogrammering' for gradvis å forbedre kodekvaliteten. Imidlertid er disse ikke erstatninger for omstrukturering; de komplementerer prosessen.

Når det gjelder implementering, tilbyr Ruby en utmerket og uttrykksfull syntaks som ofte resulterer i kortere, mer lesbar kode etter omstrukturering. Nøkkelprinsipper inkluderer DRY (Don't Repeat Yourself), å bruke meningsfulle navn, å holde metoder korte og fokuserte på en enkelt oppgave, og å bruke Rubys Enumerable-modul effektivt, som sett i eksemplet ovenfor. Automatiserte verktøy som RuboCop kan også hjelpe programmerere med å identifisere steder i koden som kunne dra nytte av omstrukturering.

## Se også

For å grave dypere inn i omstrukturering i Ruby, sjekk ut disse ressursene:

- Martin Fowlers banebrytende bok: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Rubys stilguide for å skrive renere kode: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, en statisk kodeanalysator (linter) og formatter: [RuboCop GitHub Repository](https://github.com/rubocop/rubocop)
