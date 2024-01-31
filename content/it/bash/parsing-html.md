---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:29:49.817357-07:00
html_title:           "Bash: Analisi dell'HTML"
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
L'analisi HTML consiste nel decomporre il codice HTML per estrarne dati specifici. I programmatori la eseguono per raccogliere informazioni dai siti web in modo automatico.

## How to:
Per analizzare HTML in Bash, possiamo utilizzare strumenti come `grep`, `sed`, o `awk`. Ma il migliore è `pup`, uno strumento specifico per l'HTML.

Installiamo `pup`:
```Bash
sudo apt-get install -y pup
```

Supponiamo di voler estrarre i titoli da una pagina HTML:
```Bash
curl -s https://esempio.com | pup 'h1 text{}'
```

Questo estrae tutti i testi all'interno degli elementi `<h1>`. Semplice, no?

## Deep Dive
L'analisi di HTML con i classici strumenti di testo di Unix può essere ostica. Questi strumenti non comprendono la struttura HTML, così facilmente rompono con HTML complicato.

`pup` è venuto a colmare questa lacuna, offrendo un modo specifico per analizzare HTML in linea di comando. 

Altre alternative includono: `xmllint`, `html-xml-utils` e linguaggi come Python che hanno librerie come BeautifulSoup.

Dettaglio implementativo: mentre `grep` cerca semplicemente pattern di testo, `pup` converte l'HTML in un documento DOM, che può poi navigare e manipolare.

## See Also
- Documentazione `pup`: https://github.com/ericchiang/pup
- BeautifulSoup (Python): https://www.crummy.com/software/BeautifulSoup/
- `xmllint`: http://xmlsoft.org/xmllint.html
- `html-xml-utils`: https://www.w3.org/Tools/HTML-XML-utils/
