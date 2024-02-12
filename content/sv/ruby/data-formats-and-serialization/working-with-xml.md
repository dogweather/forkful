---
title:                "Att arbeta med XML"
aliases: - /sv/ruby/working-with-xml.md
date:                  2024-01-26T04:35:23.150927-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML innebär att tolka, generera och manipulera XML (eXtensible Markup Language)-dokument med kod. Programmerare gör det för att interagera med många webbtjänster, konfigurationsfiler och datautbytesformat där XML är det gemensamma språket.

## Hur man gör:
Låt oss använda REXML, som ingår i Ruby, för att tolka ett XML-utdrag:
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
  puts "Namn: #{element.attributes['name']}, Färg: #{element.attributes['color']}"
end
```
Utskrift:
```
Namn: apple, Färg: green
Namn: banana, Färg: yellow
```

Att generera XML är också enkelt:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML-utskrift:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Fördjupning:
XML:s rötter går tillbaka till 1990-talet som en förenklad undergrupp av SGML för webbdokument. Det är ordrikt men mycket strukturerat, och det är därför det har hållit i sig. Det är inte det enda alternativet - JSON och YAML har blivit populära för sin enkelhet - men XML står starkt i många företag och legacy-system.

Ruby erbjuder några sätt att ta sig an XML. REXML är ett helt-Ruby-bibliotek som är enkelt att komma igång med. Nokogiri är en gem som omsluter snabbare C-bibliotek, vilket erbjuder hastighet och extra funktioner. Väljer mellan dem? Börja med REXML för mindre uppgifter och gå över till Nokogiri om du behöver mer kraft.

Under huven handlar tolkningen av XML om att översätta strängar till DOM- eller SAX-modeller. DOM skapar ett träd i minnet, medan SAX strömmar dokumentet och avfyrar händelser när det tolkas. REXML erbjuder båda modellerna, men tenderar att vara långsammare än C-extensioner som de som används av Nokogiri.

## Se även:
- Ruby REXML-dokumentation: https://www.rubydoc.info/stdlib/rexml
- Nokogiri gem: https://nokogiri.org/
- XML-specifikation: https://www.w3.org/XML/
- En introduktion till SAX: https://www.saxproject.org/
- Jämförelse mellan YAML vs. JSON vs. XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
