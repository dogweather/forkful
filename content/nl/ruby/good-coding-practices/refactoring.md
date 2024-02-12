---
title:                "Refactoring"
date:                  2024-01-28T22:06:06.680324-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Refactoring is het proces van het herstructureren van bestaande computercode zonder het externe gedrag ervan te wijzigen. Programmeurs voeren refactoring uit om niet-functionele attributen van de software te verbeteren, zoals leesbaarheid, verminderde complexiteit, verbeterde onderhoudbaarheid, of prestatieverbetering.

## Hoe:

Laten we een voorbeeld doorlopen van het refactoren van een Ruby-methode die de som van kwadraten berekent.

**Voor Refactoring:**
```ruby
def som_van_kwadraten(getallen)
  som = 0
  getallen.each do |getal|
    kwadraat = getal * getal
    som += kwadraat
  end
  som
end

puts som_van_kwadraten([1, 2, 3])  # Uitvoer: 14
```

**Na Refactoring:**
```ruby
def som_van_kwadraten(getallen)
  getallen.map { |getal| getal**2 }.sum
end

puts som_van_kwadraten([1, 2, 3])  # Uitvoer: 14
```

De gerefacteerde versie gebruikt Ruby Enumerables om dezelfde logica beknopter en duidelijker uit te drukken. De `map` methode transformeert elk element, en `sum` aggregeert hun waarden, waardoor de noodzaak voor handmatig lusbeheer en variabeltoewijzing overbodig wordt.

## Diepere Duik

Refactoring heeft een rijke historische context, die teruggaat tot de vroege praktijken in de softwareontwikkeling. Eerste vermeldingen zijn te traceren tot de jaren '90, met aanzienlijke bijdragen van Martin Fowler in zijn boek "Refactoring: Improving the Design of Existing Code", waar hij een catalogus van patronen voor refactoring biedt. Sindsdien is refactoring een hoeksteen van agile ontwikkelingspraktijken geworden.

Wanneer we praten over alternatieven voor refactoring, moeten we ofwel een andere benadering zoals 'Herschrijven' overwegen, waarbij je het oude systeem gedeeltelijk of geheel vervangt, of praktijken zoals 'Code Reviews' en 'Paarprogrammering' aanpassen om de codekwaliteit geleidelijk te verbeteren. Dit zijn echter geen vervangingen voor refactoring; ze vullen het proces aan.

Wat betreft implementatie, biedt Ruby een uitstekende en expressieve syntaxis die vaak resulteert in kortere, beter leesbare code na refactoring. Belangrijke principes omvatten DRY (Don't Repeat Yourself), gebruik van betekenisvolle namen, methoden kort houden en gericht op een enkele taak en effectief gebruik van Ruby's Enumerable module, zoals te zien is in het bovenstaande voorbeeld. Geautomatiseerde hulpmiddelen zoals RuboCop kunnen programmeurs ook helpen om plekken in de code te identificeren die kunnen profiteren van refactoring.

## Zie Ook

Om dieper in te gaan op refactoring in Ruby, bekijk deze bronnen:

- Het baanbrekende boek van Martin Fowler: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Ruby's stijlgids voor het schrijven van schonere code: [De Ruby Stijlgids](https://rubystyle.guide/)
- RuboCop, een statische code-analyzer (linter) en formatter: [RuboCop GitHub Repository](https://github.com/rubocop/rubocop)
