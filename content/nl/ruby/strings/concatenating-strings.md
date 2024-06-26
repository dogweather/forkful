---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:09.277637-07:00
description: "Hoe: In Ruby kun je strings aan elkaar koppelen met de `+` operator\
  \ of de `<<` methode, die de string ter plaatse wijzigt. Hier is hoe je de punten\
  \ - of\u2026"
lastmod: '2024-03-13T22:44:51.326818-06:00'
model: gpt-4-0125-preview
summary: In Ruby kun je strings aan elkaar koppelen met de `+` operator of de `<<`
  methode, die de string ter plaatse wijzigt.
title: Samenvoegen van strings
weight: 3
---

## Hoe:
In Ruby kun je strings aan elkaar koppelen met de `+` operator of de `<<` methode, die de string ter plaatse wijzigt. Hier is hoe je de punten - of beter gezegd, de woorden - met elkaar verbindt:

```Ruby
# Met behulp van de + operator, die een nieuwe string retourneert
begroeting = "Hallo, " + "wereld!"
puts begroeting # Uitvoer: Hallo, wereld!

# Met behulp van de << methode, die de originele string wijzigt
naam = "Alice"
naam << ", ontmoet Bob"
puts naam # Uitvoer: Alice, ontmoet Bob
```

## Diepgaand
Sinds de geboorte van Ruby is concatenatie deel van de taal. Maar met de tijd heeft de taal meer manieren geboden om strings samen te weven.

We hebben het gehad over `+` en `<<`, maar er is ook de `concat` methode en interpolatie.

- Gebruik van `concat`: Deze methode is vergelijkbaar met `<<` maar stelt je in staat om in één keer meerdere strings toe te voegen.
```Ruby
zinsnede = "Rozen zijn rood"
zinsnede.concat(", viooltjes zijn blauw")
puts zinsnede # Uitvoer: Rozen zijn rood, viooltjes zijn blauw
```

- Interpolatie: Zet variabelen in een string zonder ze direct te concatenaten. Het is netter en wordt de voorkeur gegeven voor het invoegen van variabelen:
```Ruby
stemming = "enthousiast"
bericht = "Ik ben #{stemming} om Ruby te leren!"
puts bericht # Uitvoer: Ik ben enthousiast om Ruby te leren!
```

Interpolatie roept automatisch `to_s` aan op elke variabele, zodat niet-stringtypes goed werken binnen een string.

Onthoud ook, het gaat niet alleen om woorden aan elkaar plakken; Ruby houdt ook rekening met prestaties. Wanneer je `+` gebruikt, maakt Ruby een nieuwe string. Op lange termijn of in loops kan dit geheugenintensief zijn. In tegenstelling, `<<` en `concat` wijzigen de originele string, wat vaak efficiënter is.

## Zie Ook
- De Ruby-documentatie over String: https://ruby-doc.org/core-3.1.2/String.html
- Een artikel over Ruby string-interpolatie: https://www.rubyguides.com/2018/11/ruby-string-interpolation/
- Een gids voor Ruby-operators: https://www.tutorialspoint.com/ruby/ruby_operators.htm
