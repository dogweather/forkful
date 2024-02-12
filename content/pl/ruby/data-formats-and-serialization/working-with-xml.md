---
title:                "Praca z XML"
aliases: - /pl/ruby/working-with-xml.md
date:                  2024-01-26T04:35:22.920382-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z XML oznacza parsowanie, generowanie i manipulowanie dokumentami XML (eXtensible Markup Language) za pomocą kodu. Programiści robią to, aby współdziałać z wieloma usługami sieciowymi, plikami konfiguracyjnymi i formatami wymiany danych, gdzie XML jest lingua franca.

## Jak to zrobić:
Użyjmy REXML, dołączonego do Ruby, aby przetworzyć fragment XML:
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
  puts "Nazwa: #{element.attributes['name']}, Kolor: #{element.attributes['color']}"
end
```
Wyjście:
```
Nazwa: apple, Kolor: zielony
Nazwa: banana, Kolor: żółty
```

Generowanie XML jest równie proste:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
Wyjście XML:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Pogłębiona analiza:
Korzenie XML sięgają lat 90. jako uproszczony podzestaw SGML dla dokumentów internetowych. Jest rozwlekły, ale bardzo strukturalny, i to dlaczego nadal się utrzymuje. Nie jest jedyną opcją – JSON i YAML stały się popularne ze względu na swoją prostotę – ale XML utrzymuje silną pozycję w wielu systemach korporacyjnych i dziedzicznych.

Ruby oferuje kilka sposobów na radzenie sobie z XML. REXML to biblioteka całkowicie w Ruby, która jest łatwa do zrozumienia. Nokogiri to gem, który opakowuje szybsze biblioteki C, oferując szybkość i dodatkowe funkcje. Co wybrać? Zacznij od REXML do mniejszych zadań i przejdź do Nokogiri, jeśli potrzebujesz więcej mocy.

Pod spodem, parsowanie XML to przekładanie ciągów znaków na modele DOM lub SAX. DOM tworzy drzewo w pamięci, podczas gdy SAX strumieniuje dokument i generuje zdarzenia w miarę parsowania. REXML oferuje oba modele, ale ma tendencję do bycia wolniejszym niż rozszerzenia C używane przez Nokogiri.

## Zobacz również:
- Dokumentacja Ruby REXML: https://www.rubydoc.info/stdlib/rexml
- Gem Nokogiri: https://nokogiri.org/
- Specyfikacja XML: https://www.w3.org/XML/
- Wprowadzenie do SAX: https://www.saxproject.org/
- Porównanie YAML vs. JSON vs. XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
