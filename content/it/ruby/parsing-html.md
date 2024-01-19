---
title:                "Analisi del html"
html_title:           "Arduino: Analisi del html"
simple_title:         "Analisi del html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è e Perchè? 

L'analisi sintattica (Parsing) di HTML consiste nell'estrarre dati utili dai documenti HTML. I programmatori la usano per automatizzare il recupero di informazioni dai siti web, per esempio per l'analisi dei dati.

## Come Fare:

Il pacchetto 'Nokogiri' è uno dei più utili per l'analisi HTML in Ruby. Installalo con:

```Ruby
gem install nokogiri
```

Ecco un esempio di come si usa:

```Ruby
require 'nokogiri'
require 'open-uri'

html = open('https://www.esempio.com')
doc = Nokogiri::HTML(html)

titoli = doc.xpath('//h1').map {|h1| h1.content}
puts titoli
```

Questo codice scarica la pagina www.esempio.com, ne estrae tutti i titoli di primo livello (h1) e li stampa.

## Approfondimenti

L'analisi sintattica HTML risale agli albori del web, quando i programmatori iniziarono ad estrarre dati da pagine web per molti scopi. 

Esistono altre librerie per l'analisi HTML in Ruby, come 'hpricot' o 'mechanize', ma 'Nokogiri' è la più popolare ed è generalmente considerata la più facile da usare.

L'implementazione di un parser HTML può variare, ma di solito si tratta di leggere la struttura ad albero del documento HTML e "navigare" tra i suoi nodi.

## Vedi Anche

- Documentazione di Nokogiri: [http://nokogiri.org](http://nokogiri.org)
- Tutorial su come fare scraping con Nokogiri: [https://www.tutorialspoint.com/ruby-on-rails/rails-web-services.htm](https://www.tutorialspoint.com/ruby-on-rails/rails-web-services.htm)
- Parsing HTML con Ruby: [https://ruby-doc.org/stdlib-2.6.1/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-2.6.1/libdoc/open-uri/rdoc/OpenURI.html)