---
title:                "Parsing di HTML"
html_title:           "Ruby: Parsing di HTML"
simple_title:         "Parsing di HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# Perché

Se stai lavorando con il web, potresti avere bisogno di estrarre informazioni da pagine web scritte in HTML. Per fare questo, è necessario imparare a usare Ruby per il parsing di HTML.

## Come fare

Per iniziare a lavorare con HTML in Ruby, è necessario utilizzare la gemma Nokogiri. Questa gemma è progettata specificamente per l'estrazione di dati da documenti HTML.

```Ruby
require 'nokogiri'
require 'open-uri'

#definisci l'URL della pagina web da analizzare
url = "https://www.example.com"

#scarica il contenuto della pagina web e crea un oggetto Nokogiri
page = Nokogiri::HTML(URI.open(url))

#usa i selettori CSS per trovare elementi specifici nella pagina
page.css(".title").each do |element|
  puts element.text #stampa il testo trovato dentro l'elemento
end
```

Questo codice scaricherà il contenuto della pagina web all'URL specificato e, utilizzando i selettori CSS, estrarre tutti gli elementi che hanno la classe "title" e stamparne il contenuto.

## Approfondimento

Mentre il codice di esempio sopra funziona bene per estrarre elementi specifici di una pagina web, ci sono molti altri modi in cui si può utilizzare Nokogiri per manipolare i documenti HTML.

Ad esempio, è possibile utilizzare il metodo `.xpath` per utilizzare espressioni XPath e ottenere elementi specifici in modo più preciso. È anche possibile utilizzare i metodi `.text` e `.attribute` per estrarre il testo e gli attributi di un elemento specifico.

Questa gemma è stata ampiamente utilizzata nella comunità Ruby per anni ed è sempre migliorata e aggiornata per mantenere la compatibilità con le ultime versioni di HTML. Ci sono anche molti tutorial e risorse online disponibili per imparare a utilizzare Nokogiri in modo efficace.

## Vedi anche

- [Documentazione ufficiale di Nokogiri](https://nokogiri.org/)
- [Tutorial di Nokogiri su RubyGuides](https://www.rubyguides.com/2018/02/parsing-html-nokogiri/)
- [Estrazione di dati da pagine web con Nokogiri su Medium](https://medium.com/@matugm/parsing-html-with-nokogiri-8a570cdfda08)