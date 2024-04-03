---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:17.966953-07:00
description: "Parsing av HTML betyr \xE5 rive fra hverandre en bit HTML-kode for \xE5\
  \ forst\xE5 dens struktur og innhold. Programmerere gj\xF8r dette for \xE5 ekstrahere\
  \ data,\u2026"
lastmod: '2024-03-13T22:44:41.315704-06:00'
model: gpt-4-0125-preview
summary: "Parsing av HTML betyr \xE5 rive fra hverandre en bit HTML-kode for \xE5\
  \ forst\xE5 dens struktur og innhold."
title: Analysering av HTML
weight: 43
---

## Hva & Hvorfor?
Parsing av HTML betyr å rive fra hverandre en bit HTML-kode for å forstå dens struktur og innhold. Programmerere gjør dette for å ekstrahere data, manipulere innhold, eller migrere informasjon mellom formater og systemer.

## Hvordan:
For å parse HTML i Ruby, installer 'Nokogiri'-gemmen med `gem install nokogiri`. Nokogiri er som en Sveitsisk lommekniv for å jobbe med HTML og XML i Ruby. Her er et kjapt eksempel:

```ruby
require 'nokogiri'
require 'open-uri'

# Last inn HTML-innhold fra et nettsted
html_content = URI.open('http://example.com').read

# Parse HTML-en
doc = Nokogiri::HTML(html_content)

# Ekstraher tittelen
title = doc.xpath('//title').text
puts "Tittelen på siden er: #{title}"
```

Dette gir ut noe som: `Tittelen på siden er: Eksempeldomene`.

## Dypdykk
Tilbake i de tidlige Ruby-dagene, var alternativene for parsing av HTML begrensede. REXML var innebygd men treg. Så dukket Hpricot opp, men det falmet bort. Nokogiri debuterte i 2008, og blandet enkelheten til Hpricot med hastigheten og kraften til libxml, et bevist XML-verktøy.

I parsing-verdenen er det alltid alternativer. Noen sverger til det innebygde 'rexml'-biblioteket eller 'oga', en annen XML/HTML-parser for Ruby. Men Nokogiri forblir en favoritt for sin robusthet og hastighet, for ikke å nevne dens enorme utvalg av funksjoner.

Under panseret konverterer Nokogiri HTML til et Document Object Model (DOM)—en trestruktur. Dette gjør det enkelt å navigere og manipulere elementer. Ved å bruke XPath og CSS-selektorer, kan du peke ut akkurat den informasjonen du trenger.

## Se Også
- Nokogiri-gem: [https://nokogiri.org/](https://nokogiri.org/)
- Ruby sin rexml-dokumentasjon: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Alternativ parser 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Lær om XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
