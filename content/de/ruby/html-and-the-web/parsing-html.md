---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:01.919353-07:00
description: "Wie: Um HTML in Ruby zu parsen, installiere das 'Nokogiri'-Gem mit `gem\
  \ install nokogiri`. Nokogiri ist wie ein Schweizer Taschenmesser f\xFCr die Arbeit\
  \ mit\u2026"
lastmod: '2024-03-13T22:44:54.398413-06:00'
model: gpt-4-0125-preview
summary: Um HTML in Ruby zu parsen, installiere das 'Nokogiri'-Gem mit `gem install
  nokogiri`.
title: HTML parsen
weight: 43
---

## Wie:
Um HTML in Ruby zu parsen, installiere das 'Nokogiri'-Gem mit `gem install nokogiri`. Nokogiri ist wie ein Schweizer Taschenmesser für die Arbeit mit HTML und XML in Ruby. Hier ein schnelles Beispiel:

```ruby
require 'nokogiri'
require 'open-uri'

# HTML-Inhalt von einer Webseite laden
html_inhalt = URI.open('http://example.com').read

# Das HTML parsen
doc = Nokogiri::HTML(html_inhalt)

# Den Titel extrahieren
titel = doc.xpath('//title').text
puts "Der Titel der Seite ist: #{titel}"
```

Das erzeugt etwa so etwas: `Der Titel der Seite ist: Beispiel Domain`.

## Tiefer Eintauchen
In den frühen Tagen von Ruby waren die Optionen für das Parsen von HTML begrenzt. REXML war eingebaut, aber langsam. Dann kam Hpricot auf, aber es verlor an Beliebtheit. Nokogiri debütierte 2008 und verband die Einfachheit von Hpricot mit der Geschwindigkeit und Leistung von libxml, einem bewährten XML-Toolkit.

In der Welt des Parsens gibt es immer Alternativen. Einige schwören auf die eingebaute 'rexml'-Bibliothek oder 'oga', einen weiteren XML/HTML-Parser für Ruby. Aber Nokogiri bleibt aufgrund seiner Robustheit und Geschwindigkeit, ganz zu schweigen von seiner riesigen Auswahl an Funktionen, ein Favorit.

Unter der Haube konvertiert Nokogiri HTML in ein Document Object Model (DOM) – eine Baumstruktur. Das erleichtert es, Elemente zu navigieren und zu manipulieren. Mit XPath und CSS-Selektoren können Sie jedes benötigte Informationsstück genau lokalisieren.

## Siehe auch
- Nokogiri-Gem: [https://nokogiri.org/](https://nokogiri.org/)
- Dokumentation von Rubys rexml: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Alternativer Parser 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Lerne über XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
