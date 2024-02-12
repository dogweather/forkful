---
title:                "Å jobbe med XML"
aliases: - /no/ruby/working-with-xml.md
date:                  2024-01-26T04:35:18.429329-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å jobbe med XML betyr å parse, generere og manipulere XML (eXtensible Markup Language)-dokumenter ved hjelp av kode. Programmerere gjør dette for å interagere med mange netttjenester, konfigurasjonsfiler og datautvekslingsformater der XML er lingua franca.

## Hvordan:
La oss bruke REXML, som er inkludert med Ruby, for å parse et XML-snutt:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') do |element|
  puts "Navn: #{element.attributes['name']}, Farge: #{element.attributes['color']}"
end
```
Utdata:
```
Navn: apple, Farge: green
Navn: banana, Farge: yellow
```

Å generere XML er også rett frem:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML Utdata:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Dypdykk:
XML sine røtter går tilbake til 1990-tallet som en forenklet delmengde av SGML for nettdokumenter. Det er ordrikt, men høyst strukturert, og det er derfor det har holdt seg. Det er ikke det eneste alternativet – JSON og YAML har blitt populære for sin enkelhet – men XML holder seg sterkt i mange bedrifts- og arvesystemer.

Ruby gir noen måter å takle XML på. REXML er et helt-Ruby-bibliotek som er enkelt å hoppe inn i. Nokogiri er en perle som pakker hurtigere C-biblioteker, og tilbyr fart og ekstra funksjoner. Velge mellom dem? Start med REXML for mindre oppgaver og gå over til Nokogiri hvis du trenger mer kraft.

Under panseret handler parsing av XML om å oversette strenger til DOM- eller SAX-modeller. DOM skaper et tre i minnet, mens SAX streamer dokumentet og utløser hendelser mens det parses. REXML tilbyr begge modeller, men har en tendens til å være tregere enn C-utvidelser som de som brukes av Nokogiri.

## Se også:
- Ruby REXML-dokumentasjon: https://www.rubydoc.info/stdlib/rexml
- Nokogiri perle: https://nokogiri.org/
- XML-spesifikasjon: https://www.w3.org/XML/
- En introduksjon til SAX: https://www.saxproject.org/
- Sammenligning av YAML vs. JSON vs. XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
