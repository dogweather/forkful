---
title:                "Lavorare con XML"
date:                  2024-01-26T04:35:13.837438-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con l'XML significa analizzare, generare e manipolare documenti XML (eXtensible Markup Language) utilizzando il codice. I programmatori lo fanno per interagire con molti servizi web, file di configurazione e formati di scambio dati dove l'XML è la lingua franca.

## Come fare:
Usiamo REXML, incluso con Ruby, per analizzare uno snippet XML:
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
  puts "Name: #{element.attributes['name']}, Color: #{element.attributes['color']}"
end
```
Output:
```
Name: apple, Color: verde
Name: banana, Color: giallo
```

La generazione di XML è altrettanto diretta:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
Output XML:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## Approfondimento:
Le radici dell'XML risalgono agli anni '90 come un sottoinsieme semplificato di SGML per i documenti web. È verboso ma altamente strutturato, ed è per questo che è rimasto in uso. Non è l'unico sistema disponibile: JSON e YAML sono diventati popolari per la loro semplicità, ma l'XML rimane forte in molti sistemi enterprise e legacy.

Ruby offre alcuni modi per affrontare l'XML. REXML è una libreria totalmente in Ruby che è facile da iniziare a usare. Nokogiri è una gemma che avvolge librerie C più rapide, offrendo velocità e funzionalità extra. Da scegliere tra i due? Inizia con REXML per compiti più piccoli e passa a Nokogiri se hai bisogno di più potenza.

Sotto il cofano, analizzare l'XML riguarda la traduzione di stringhe in modelli DOM o SAX. DOM crea un albero in memoria, mentre SAX trasmette il documento e genera eventi man mano che viene analizzato. REXML offre entrambi i modelli, ma tende ad essere più lento rispetto alle estensioni C come quelle utilizzate da Nokogiri.

## Vedi anche:
- Documentazione Ruby REXML: https://www.rubydoc.info/stdlib/rexml
- Gemma Nokogiri: https://nokogiri.org/
- Specifica XML: https://www.w3.org/XML/
- Un'introduzione a SAX: https://www.saxproject.org/
- Confronto tra YAML, JSON e XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
