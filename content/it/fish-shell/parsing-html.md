---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:31:17.849235-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Il parsing HTML consiste nell'analizzare il codice HTML per estrarre dati o comprendere la struttura della pagina. I programmatori lo fanno per automatizzare l'interazione con i siti web, raccogliere informazioni o testare il contenuto.

## How to:
Fish non è il linguaggio ideale per il parsing HTML, ma potete usare strumenti di riga di comando come `pup`, un parser HTML che si integra bene con Fish. Installiamo `pup` e proviamolo.

```Fish Shell
# Installazione di pup su sistemi basati su Debian
sudo apt install pup

# Utilizzando pup per estrarre titoli da un file HTML
cat index.html | pup 'h1 text{}'
```

Output:
```
Il mio titolo di pagina
```

## Deep Dive
Fish Shell non è stato progettato per il parsing HTML; è ottimizzato per la gestione di compiti di shell e pipeline. In passato, i programmatori usavano espressioni regolari per il parsing HTML, ma questo approccio è notoriamente fragile e complicato. Strumenti come `pup`, `beautifulsoup` (Python) o `nokogiri` (Ruby) sono ora preferiti perché analizzano HTML in modo più strutturato e sicuro, rispettando la complessità del linguaggio HTML. `pup` si ispira a `jq` per il JSON, offrendo una sintassi dichiarativa per navigare e manipolare HTML.

## See Also:
- Documentazione di `pup`: https://github.com/ericchiang/pup
- Tutorial su BeautifulSoup per Python: https://www.crummy.com/software/BeautifulSoup/bs4/doc/
- Documentazione di `nokogiri` per Ruby: https://nokogiri.org/