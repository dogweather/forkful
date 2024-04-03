---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:17.938032-07:00
description: "Controleren of een map bestaat betekent bevestigen of een map aanwezig\
  \ is in het bestandssysteem. Programmeurs doen dit om fouten te voorkomen voordat\
  \ ze\u2026"
lastmod: '2024-03-13T22:44:51.360177-06:00'
model: gpt-4-0125-preview
summary: Controleren of een map bestaat betekent bevestigen of een map aanwezig is
  in het bestandssysteem.
title: Controleren of een directory bestaat
weight: 20
---

## Wat & Waarom?
Controleren of een map bestaat betekent bevestigen of een map aanwezig is in het bestandssysteem. Programmeurs doen dit om fouten te voorkomen voordat ze bestandsoperaties proberen uit te voeren, zoals het lezen van of schrijven naar een map.

## Hoe te:

In Ruby kun je de methode `Dir.exist?` gebruiken om te controleren of een map bestaat. Zo ziet het eruit:

```ruby
if Dir.exist?("/pad/naar/map")
  puts "Map bestaat!"
else
  puts "Zo'n map bestaat niet."
end
```

Voorbeelduitvoer wanneer de map bestaat:

```
Map bestaat!
```

En wanneer dat niet zo is:

```
Zo'n map bestaat niet.
```

## Diepere Duik

Historisch gezien heeft Ruby meerdere manieren geboden om met het bestandssysteem te interageren. `Dir.exist?` is nu een voorkeursmethode vanwege de duidelijkheid en eenvoud, maar oudere code kan `File.directory?` gebruiken. Beide methoden betekenen vrijwel hetzelfde en zijn min of meer inwisselbaar.

```ruby
# Hetzelfde resultaat behalen met File.directory?
if File.directory?("/pad/naar/map")
  puts "Map bestaat!"
else
  puts "Zo'n map bestaat niet."
end
```

Waarom de redundantie? Het is een deel van Ruby's principe om programmeurs meer dan één manier te bieden om iets te doen. Toch kan `Dir.exist?` worden beschouwd als een semantisch nauwkeurigere manier om specifiek naar mappen te zoeken.

Wat betreft de implementatie onder de motorkap, wanneer je `Dir.exist?` aanroept, vraagt Ruby het besturingssysteem om het bestandssysteem te controleren, dat controleert of het gespecificeerde pad naar een map wijst.

Wat alternatieven betreft, naast handmatige padcontrole, zou je ook de uitzonderingen kunnen vangen die het gevolg zijn van het proberen toegang te krijgen tot een niet-bestaande map. Dit wordt echter niet aanbevolen omdat het duurder is in termen van systeembronnen en minder duidelijk voor iemand die de code leest.

## Zie Ook

Om verder in te gaan op Ruby's bestands- en mapafhandeling, bekijk de volgende bronnen:

- Ruby Docs over de `Dir` klasse: [https://ruby-doc.org/core/Dir.html](https://ruby-doc.org/core/Dir.html)
- Ruby Docs over de `File` klasse: [https://ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Voor goede codeerpraktijken over foutafhandeling: [https://www.honeybadger.io/blog/ruby-exception-vs-standarderror-whats-the-difference/](https://www.honeybadger.io/blog/ruby-exception-vs-standarderror-whats-the-difference/)
