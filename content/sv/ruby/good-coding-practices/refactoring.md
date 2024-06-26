---
date: 2024-01-26 03:36:57.429410-07:00
description: "Hur man g\xF6r: L\xE5t oss g\xE5 igenom ett exempel p\xE5 refaktorisering\
  \ av en Ruby-metod som ber\xE4knar summan av kvadrater. **F\xF6re refaktorisering:**."
lastmod: '2024-03-13T22:44:38.439607-06:00'
model: gpt-4-0125-preview
summary: "L\xE5t oss g\xE5 igenom ett exempel p\xE5 refaktorisering av en Ruby-metod\
  \ som ber\xE4knar summan av kvadrater."
title: Refaktorisering
weight: 19
---

## Hur man gör:
Låt oss gå igenom ett exempel på refaktorisering av en Ruby-metod som beräknar summan av kvadrater.

**Före refaktorisering:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # Output: 14
```

**Efter refaktorisering:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # Output: 14
```

Den refaktoriserade versionen använder Ruby:s Enumerables för att uttrycka samma logik på ett mer koncist och klart sätt. `map`-metoden transformerar varje element, och `sum` aggregerar deras värden, vilket eliminerar behovet av manuell loop-hantering och variabeltilldelning.

## Djupdykning
Refaktorisering har en rik historisk kontext, som går tillbaka till de tidiga metoderna i mjukvaruutveckling. De första omnämnandena kan spåras tillbaka till 1990-talet, med betydande bidrag från Martin Fowler i hans bok "Refactoring: Improving the Design of Existing Code", där han tillhandahåller en katalog av mönster för refaktorisering. Sedan dess har refaktorisering blivit en hörnsten i agila utvecklingspraxis.

När vi pratar om alternativ till refaktorisering måste vi antingen överväga en annan inställning som 'Omskrivning', där du ersätter det gamla systemet delvis eller helt eller anpassa praxis som 'Kodgranskningar' och 'Parprogrammering' för att gradvis förbättra kodkvaliteten. Dock är dessa inte ersättningar för refaktorisering; de kompletterar processen.

När det gäller implementering erbjuder Ruby en utmärkt och uttrycksfull syntax som ofta resulterar i kortare, mer läsbar kod efter refaktorisering. Nyckelprinciper inkluderar DRY (Don't Repeat Yourself), att använda meningsfulla namn, att hålla metoder korta och fokuserade på en enstaka uppgift, och att använda Rubys Enumerable-modul effektivt, som sett i exemplet ovan. Automatiserade verktyg som RuboCop kan också hjälpa programmerare att identifiera punkter i koden som skulle kunna dra nytta av refaktorisering.

## Se även
För att fördjupa dig i refaktorisering i Ruby, kolla in dessa resurser:

- Martin Fowlers banbrytande bok: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Rubys stilguide för att skriva renare kod: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, en statisk kodanalysator (lintare) och formatterare: [RuboCop GitHub-repositorium](https://github.com/rubocop/rubocop)
