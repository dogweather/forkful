---
title:                "Analisi dell'html"
html_title:           "Ruby: Analisi dell'html"
simple_title:         "Analisi dell'html"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/parsing-html.md"
---

{{< edit_this_page >}}

# Che cosa & Perché?
Il parsing di HTML è il processo di analisi di un documento HTML per estrarre le informazioni strutturate al suo interno. I programmatori fanno questo per ottenere i dati necessari da un sito web per l'utilizzo in un'applicazione o per la creazione di un altro sito web.

# Come fare:
```Ruby 
require ‘nokogiri’
require ‘open-uri’

# Creare uno scraper per estrarre i dati
url = "www.example.com"
page = Nokogiri::HTML(open(url))
# Utilizzare i selettori CSS per estrarre le informazioni desiderate
page.css(".product-name").each do |item|
  puts item.text.strip
end
```

```Ruby
# Analizzare un documento HTML salvato localmente
file = File.open("index.html")
doc = Nokogiri::HTML(file)
# Ottenere il contenuto dell'elemento specificato
item = doc.at_css("#header").inner_html
puts item
```

Output:
```
Item Name
<h1>Header</h1>
```

# Approfondimento:
Il parsing di HTML è stato introdotto nei primi anni di sviluppo del web per consentire ai programmatori di accedere ai contenuti dei siti web e utilizzarli per scopi diversi. Oltre a Nokogiri, ci sono altre librerie come Hpricot e lo standard Ruby's libxml utilizzabili per il parsing di HTML.

# Vedi anche:
- [Documentazione ufficiale di Nokogiri](https://nokogiri.org/)
- [Tutorial di parsing di HTML con Ruby](https://www.rubyguides.com/2012/01/parsing-html-in-ruby/)