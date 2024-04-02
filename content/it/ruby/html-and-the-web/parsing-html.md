---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:59.740001-07:00
description: "Analizzare l'HTML significa smontare un pezzo di codice HTML per comprenderne\
  \ la struttura e il contenuto. I programmatori lo fanno per estrarre dati,\u2026"
lastmod: '2024-03-13T22:44:44.049295-06:00'
model: gpt-4-0125-preview
summary: "Analizzare l'HTML significa smontare un pezzo di codice HTML per comprenderne\
  \ la struttura e il contenuto. I programmatori lo fanno per estrarre dati,\u2026"
title: Analisi del HTML
weight: 43
---

## Cosa e perché?
Analizzare l'HTML significa smontare un pezzo di codice HTML per comprenderne la struttura e il contenuto. I programmatori lo fanno per estrarre dati, manipolare contenuti o migrare informazioni tra formati e sistemi.

## Come fare:
Per analizzare l'HTML in Ruby, installa la gemma 'Nokogiri' con `gem install nokogiri`. Nokogiri è come un coltellino svizzero per lavorare con HTML e XML in Ruby. Ecco un esempio rapido:

```ruby
require 'nokogiri'
require 'open-uri'

# Carica il contenuto HTML da un sito web
html_content = URI.open('http://example.com').read

# Analizza l'HTML
doc = Nokogiri::HTML(html_content)

# Estrai il titolo
title = doc.xpath('//title').text
puts "Il titolo della pagina è: #{title}"
```

Questo produrrà qualcosa del tipo: `Il titolo della pagina è: Dominio di esempio`.

## Approfondimento
Nei primi tempi di Ruby, le opzioni per l'analisi dell'HTML erano limitate. REXML era integrato ma lento. Poi è arrivato Hpricot, ma si è rapidamente esaurito. Nokogiri ha debuttato nel 2008, combinando la facilità di Hpricot con la velocità e la potenza di libxml, un toolkit XML collaudato.

Nel mondo dell'analisi, ci sono sempre alternative. Alcuni giurano sulla libreria integrata 'rexml' o su 'oga', un altro parser XML/HTML per Ruby. Ma Nokogiri rimane il preferito per la sua robustezza e velocità, per non parlare della sua vasta gamma di funzionalità.

Sotto il cofano, Nokogiri converte l'HTML in un Document Object Model (DOM)—una struttura ad albero. Questo rende facile navigare e manipolare gli elementi. Utilizzando XPath e i selettori CSS, puoi individuare qualsiasi informazione di cui hai bisogno.

## Vedi anche
- Gemma Nokogiri: [https://nokogiri.org/](https://nokogiri.org/)
- Documentazione di rexml di Ruby: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Parser alternativo 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Scopri XPath: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
