---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:57.151866-07:00
description: "Het vergelijken van twee datums betekent controleren of ze gelijk zijn,\
  \ of uitzoeken welke eerder of later komt. Programmeurs doen dit om gebeurtenissen\u2026"
lastmod: '2024-03-13T22:44:51.357940-06:00'
model: gpt-4-0125-preview
summary: Het vergelijken van twee datums betekent controleren of ze gelijk zijn, of
  uitzoeken welke eerder of later komt.
title: Twee datums vergelijken
weight: 27
---

## Wat & Waarom?

Het vergelijken van twee datums betekent controleren of ze gelijk zijn, of uitzoeken welke eerder of later komt. Programmeurs doen dit om gebeurtenissen bij te houden, reserveringen te beheren, tijdlijnen te sorteren en bij elke taak waar de volgorde van tijd belangrijk is.

## Hoe te:

Ruby vereenvoudigt ons leven met de Date klasse. Laten we het in actie zien.

```ruby
require 'date'

datum1 = Date.new(2023, 3, 14)
datum2 = Date.new(2023, 3, 15)

puts datum1 == datum2   # Uitvoer: false
puts datum1 != datum2   # Uitvoer: true
puts datum1 < datum2    # Uitvoer: true
puts datum1 > datum2    # Uitvoer: false
puts datum1 <= Date.today # Uitvoer: afhankelijk van de datum van vandaag
puts datum1 >= Date.today # Uitvoer: afhankelijk van de datum van vandaag
```

## Diepere duik

Datumvergelijking is niet nieuw. Het is fundamenteel, net als het vergelijken van gehele getallen, maar lastiger omdat datums onderdelen hebben—dagen, maanden, jaren. In Ruby draagt de Date klasse (uit de standaardbibliotheek) het gewicht, en houdt rekening met maanden, schrikkeljaren, enz.

Je hebt de basisvergelijkingen gezien: `==`, `!=`, `<`, `>`, `<=`, `>=`. Maar Ruby heeft meer achter de hand.

* `Date.parse` kan datums in stringvorm begrijpen en omzetten.
* `DateTime` biedt meer precisie, met ondersteuning voor tijd en tijdzone.
* Bibliotheken zoals 'ActiveSupport' (van Rails) voegen nog meer datumgerelateerde methoden toe.

Let op valkuilen:
* Tijdzones kunnen je in de war brengen als je niet voorzichtig bent.
* Schrikkelseconden worden niet meegerekend in Ruby's standaard Date/DateTime klassen.

Alternatieven voor de Date klasse zijn:

* Het gebruik van timestamps en deze vergelijken als getallen.
* De 'time' bibliotheek voor geavanceerdere tijdafhandeling.

Vergelijkingen worden snel complex. Wat als je plant en datumbereiken moet vergelijken, of terugkerende evenementen moet afhandelen? Hogere abstracties gebouwd op Ruby's Date en Time zijn vaak nodig. ActiveRecord's `between?` methode of gems zoals 'IceCube' voor terugkerende evenementen kunnen veel tijd en hoofdpijn besparen.

## Zie ook

- ActiveSupport's uitbreidingen: [Active Support Core Extensions](https://edgeguides.rubyonrails.org/active_support_core_extensions.html)
- 'IceCube' gem voor terugkerende evenementen: [IceCube](https://github.com/seejohnrun/ice_cube)
- Uitgebreide gids voor tijdzones in Ruby: [Gidsen voor tijdzones](https://thoughtbot.com/blog/its-about-time-zones)
