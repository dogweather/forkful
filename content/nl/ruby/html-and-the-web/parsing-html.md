---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:52.125030-07:00
description: "Hoe: Om in Ruby HTML te parsen, installeer je de 'Nokogiri' gem met\
  \ `gem install nokogiri`. Nokogiri is als een Zwitsers zakmes voor het werken met\
  \ HTML\u2026"
lastmod: '2024-03-13T22:44:51.335640-06:00'
model: gpt-4-0125-preview
summary: Om in Ruby HTML te parsen, installeer je de 'Nokogiri' gem met `gem install
  nokogiri`.
title: HTML Parsen
weight: 43
---

## Hoe:
Om in Ruby HTML te parsen, installeer je de 'Nokogiri' gem met `gem install nokogiri`. Nokogiri is als een Zwitsers zakmes voor het werken met HTML en XML in Ruby. Hier is een snel voorbeeld:

```ruby
require 'nokogiri'
require 'open-uri'

# Laad HTML-inhoud van een website
html_content = URI.open('http://example.com').read

# Parseer de HTML
doc = Nokogiri::HTML(html_content)

# Extraheer de titel
title = doc.xpath('//title').text
puts "De titel van de pagina is: #{title}"
```

Dit geeft iets zoals: `De titel van de pagina is: Voorbeeld Domein`.

## Diepere Duik
In de vroege dagen van Ruby waren de opties voor HTML-parsing beperkt. REXML was ingebouwd maar langzaam. Toen verscheen Hpricot, maar dat viel weg. Nokogiri debuteerde in 2008, en combineerde de gebruiksvriendelijkheid van Hpricot met de snelheid en kracht van libxml, een bewezen XML-toolkit.

In de wereld van het parsen zijn er altijd alternatieven. Sommigen zweren bij de ingebouwde 'rexml'-bibliotheek of 'oga', een andere XML/HTML-parser voor Ruby. Maar Nokogiri blijft favoriet vanwege zijn robuustheid en snelheid, om nog maar te zwijgen van de uitgebreide reeks functies.

Onder de motorkap zet Nokogiri HTML om in een Document Object Model (DOM)—een boomstructuur. Dit maakt het gemakkelijk om elementen te navigeren en te manipuleren. Met behulp van XPath en CSS-selectors kun je elk stukje informatie dat je nodig hebt precies lokaliseren.

## Zie Ook
- Nokogiri gem: [https://nokogiri.org/](https://nokogiri.org/)
- Ruby's rexml documentatie: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Alternatieve parser 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Leer over XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
