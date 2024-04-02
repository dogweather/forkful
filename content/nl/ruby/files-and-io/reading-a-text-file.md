---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:20.772742-07:00
description: "Het lezen van een tekstbestand betekent het toegang krijgen tot de inhoud\
  \ van een bestand dat op de schijf is opgeslagen via code. Programmeurs doen dit\u2026"
lastmod: '2024-03-13T22:44:51.370671-06:00'
model: gpt-4-0125-preview
summary: "Het lezen van een tekstbestand betekent het toegang krijgen tot de inhoud\
  \ van een bestand dat op de schijf is opgeslagen via code. Programmeurs doen dit\u2026"
title: Een tekstbestand lezen
weight: 22
---

## Wat & Waarom?
Het lezen van een tekstbestand betekent het toegang krijgen tot de inhoud van een bestand dat op de schijf is opgeslagen via code. Programmeurs doen dit om gegevens binnen hun applicaties te verwerken, analyseren of weer te geven.

## Hoe:

Een bestand lezen in Ruby is eenvoudig. Je kunt de `File` klasse gebruiken, die verschillende methoden biedt om bestanden te lezen. Hier is een simpel voorbeeld van het lezen van een heel bestand:

```Ruby
File.open("voorbeeld.txt", "r") do |bestand|
  puts bestand.read
end
```

Als `voorbeeld.txt` de tekst "Hallo, Ruby!" bevat, dan krijg je dit:

```
Hallo, Ruby!
```

Om regel voor regel te lezen:

```Ruby
File.foreach("voorbeeld.txt") { |regel| puts regel }
```

Zelfde `voorbeeld.txt`, nu zal de uitvoer regel voor regel zijn:

```
Hallo, Ruby!
```

## Diepgaand:

Historisch gezien is het lezen van bestanden een kernfunctie van programmeertalen geweest, waardoor interacties met het bestandssysteem mogelijk zijn.

In Ruby kun je ook op verschillende manieren een bestand lezen:

1. `IO` klasse: Voor low-level bestandsbewerkingen.
2. `readlines` methode: Laadt het hele bestand in een array, met elke regel als een element.
3. `File.read`: Snelle manier om een heel bestand in een string te lezen.

Er is een afweging te overwegen: `File.read` is netjes voor kleine bestanden, maar het kan geheugenintensief zijn voor grotere. Dat is wanneer het lezen regel voor regel of in stukken waardevol wordt.

## Zie ook:

- Ruby Docs voor de `File` klasse: [ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
- Stack Overflow discussies over het lezen van bestanden in Ruby: [stackoverflow.com/questions/tagged/ruby+file-io](https://stackoverflow.com/questions/tagged/ruby+file-io)
