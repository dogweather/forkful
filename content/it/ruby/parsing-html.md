---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:33:42.179981-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Il parsing di HTML consiste nel trasformare il codice HTML in una struttura comprensibile per il programma che lo legge. I programmatori lo fanno per estrarre dati, automatizzare il controllo dei contenuti o manipolare pagine web.

## How to:
In Ruby, useremo Nokogiri, una gemma popolare per il parsing HTML.

```Ruby
require 'nokogiri'
require 'open-uri'

# Carichiamo un documento HTML da un URL
html_content = URI.open('http://example.com').read
document = Nokogiri::HTML(html_content)

# Otteniamo il titolo della pagina
title = document.css('title').text
puts "Il titolo della pagina è: '#{title}'"

# Cerchiamo tutti i link presenti nella pagina
document.css('a').each do |link|
  puts "Testo: #{link.text}, URL: #{link['href']}"
end
```

Esempio di output:
```
Il titolo della pagina è: 'Example Domain'
Testo: More information, URL: https://www.iana.org/domains/example
```

## Deep Dive:
Nokogiri non è l'unico modo per fare parsing di HTML in Ruby, ma è tra i più popolari. Emerse nei primi anni 2000 permettendo un accesso più facile e veloce al DOM HTML, rispetto ad altre gemme come Hpricot (ora non più mantenuta). Nokogiri sfrutta le librerie come libxml2 per offrire parsing veloce e ben supportato.

Altre opzioni includono Oga e Loofah, ma Nokogiri rimane la scelta prediletta per molti grazie alla sua robustezza e alla ricca documentazione. Il parsing è anche un'attività soggetta a problemi di sicurezza: è importante pulire i dati HTML per evitare vulnerabilità come gli attacchi XSS.

## See Also:
- [Nokogiri Official Site](https://nokogiri.org/)
