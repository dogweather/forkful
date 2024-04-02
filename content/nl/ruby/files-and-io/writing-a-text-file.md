---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:33.778109-07:00
description: "Naar een tekstbestand schrijven in Ruby betekent gegevens opslaan in\
  \ een bestand op je systeem. Programmeurs doen dit voor gegevenspersistentie, loggen,\u2026"
lastmod: '2024-03-13T22:44:51.373153-06:00'
model: gpt-4-0125-preview
summary: "Naar een tekstbestand schrijven in Ruby betekent gegevens opslaan in een\
  \ bestand op je systeem. Programmeurs doen dit voor gegevenspersistentie, loggen,\u2026"
title: Een tekstbestand schrijven
weight: 24
---

## Wat & Waarom?

Naar een tekstbestand schrijven in Ruby betekent gegevens opslaan in een bestand op je systeem. Programmeurs doen dit voor gegevenspersistentie, loggen, en het delen van data tussen verschillende programma's of programmalooptijden.

## Hoe te:

Om naar een tekstbestand in Ruby te schrijven, gebruik je de `File` klasse. Hier is een snel voorbeeld:

```Ruby
File.open("output.txt", "w") do |file|
  file.puts "Hallo, Ruby!"
end
```

Voorbeelduitvoer (inhoud van `output.txt`):
```
Hallo, Ruby!
```

Om aan een bestaand bestand toe te voegen, gebruik je de "a" modus:

```Ruby
File.open("output.txt", "a") do |file|
  file.puts "Deze regel aan het toevoegen."
end
```

Uitvoer (aanvullende inhoud van `output.txt`):
```
Deze regel aan het toevoegen.
```

## Diepere Duik

Ruby's bestandsbehandeling is geworteld in UNIX-bestand I/O-operaties. De `open` methode kan een blok nemen, waarna het bestand automatisch gesloten wordt, wat uniek en handig is in vergelijking met sommige andere talen. Alternatieven voor `File.open` zijn onder meer `IO.write` voor snelle schrijfacties en verschillende bibliotheken zoals `CSV` of `FileUtils` voor gespecialiseerde taken.

Wanneer je naar een bestand schrijft, let dan op de karaktercodering en regelafsluitingen, vooral als je bestand gelezen moet worden door verschillende systemen of talen.

## Zie Ook

- Ruby's IO klasse: https://ruby-doc.org/core/IO.html
- Ruby's FileUtils: https://ruby-doc.org/stdlib/libdoc/fileutils/rdoc/FileUtils.html
- Ruby-Doc Bestandsklasse: https://ruby-doc.org/core/File.html
