---
title:                "Samenvoegen van strings"
date:                  2024-01-28T21:57:09.277637-07:00
model:                 gpt-4-0125-preview
simple_title:         "Samenvoegen van strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/ruby/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Strings aan elkaar plakken is gewoon een chique manier om te zeggen 'ze achter elkaar plakken'. Programmeurs doen dit om woorden en zinnen te combineren, om berichten op te bouwen, of om dynamisch waarden in tekst in te voegen.

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
