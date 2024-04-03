---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:51.709248-07:00
description: "Het converteren van een datum naar een string zet het datumobject om\
  \ in tekst die we kunnen lezen en gebruiken. Programmeurs doen dit om datums in\
  \ een\u2026"
lastmod: '2024-03-13T22:44:51.350173-06:00'
model: gpt-4-0125-preview
summary: Het converteren van een datum naar een string zet het datumobject om in tekst
  die we kunnen lezen en gebruiken.
title: Een datum converteren naar een string
weight: 28
---

## Hoe:
Ruby maakt het super eenvoudig om met datums en strings te spelen. Hier is hoe je het doet:

```Ruby
require 'date'

# Laten we een datumobject maken
my_date = Date.new(2023, 4, 14)

# Standaardconversie naar string
date_string = my_date.to_s
puts date_string  # Uitvoer: "2023-04-14"

# Aangepaste formaten met strftime (stringformat tijd)
pretty_date = my_date.strftime('%B %d, %Y')
puts pretty_date  # Uitvoer: "April 14, 2023"

# Nog een voorbeeld, gewoon voor de lol
fun_date_format = my_date.strftime('%d-%m-%Y')
puts fun_date_format  # Uitvoer: "14-04-2023"
```

## Diepgaande duik
Lang geleden schreven mensen de datum met de hand. In de programmeerwereld bracht Ruby's `Date` klasse ons de kracht om met datums om te gaan zonder te zweten. Je hebt methoden zoals `to_s` en `strftime` om je `Date` objecten in strings om te zetten.

De `to_s` methode geeft je een snelle ISO 8601 representatie (`JJJJ-MM-DD`), wat geweldig is voor een eenvoudige conversie. Maar wanneer je datum zich moet opdoffen, laat `strftime` je het exacte patroon kiezen dat je string zal volgen. Symbolen in `strftime` zoals `%Y` voor het viercijferige jaar, `%m` voor de tweecijferige maand en `%d` voor de tweecijferige dag zijn je bouwstenen om datums te formatteren.

Hoewel Ruby's `Date` en `Time` klassen solide zijn, kunnen gems zoals `Timecop` voor tijdreizen (niet echt tijdreizen, sorry) tijdens tests, of `Chronic` voor het parsen van natuurlijke taal datums, wat extra kracht geven wanneer je het nodig hebt.

De kern van de zaak? Ruby gebruikt systeembibliotheken - zoals de tijdwaarnemingsdelen van C-bibliotheken - onder de motorkap. Dat betekent dat het snel en betrouwbaar is, en eigenaardigheden zoals schrikkeljaren en zomertijd als een kampioen afhandelt.

## Zie ook
Bekijk deze bronnen voor meer details:
- Documentatie van Ruby's `Date` klasse: [ruby-doc.org/stdlib-2.7.3/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/Date.html)
- Richtlijnen van Ruby's `strftime`: [apidock.com/ruby/DateTime/strftime](https://apidock.com/ruby/DateTime/strftime)
- Gems voor meer datum-/tijdmagie: [github.com/travisjeffery/timecop](https://github.com/travisjeffery/timecop) en [github.com/mojombo/chronic](https://github.com/mojombo/chronic)
