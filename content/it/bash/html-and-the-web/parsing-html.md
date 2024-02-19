---
aliases:
- /it/bash/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:35.633354-07:00
description: "L'analisi dell'HTML significa setacciare la struttura e il contenuto\
  \ di un file HTML per estrarre informazioni. I programmatori lo fanno per accedere\
  \ ai\u2026"
lastmod: 2024-02-18 23:08:56.050562
model: gpt-4-0125-preview
summary: "L'analisi dell'HTML significa setacciare la struttura e il contenuto di\
  \ un file HTML per estrarre informazioni. I programmatori lo fanno per accedere\
  \ ai\u2026"
title: Analisi del HTML
---

{{< edit_this_page >}}

## Cosa e Perché?

L'analisi dell'HTML significa setacciare la struttura e il contenuto di un file HTML per estrarre informazioni. I programmatori lo fanno per accedere ai dati, manipolare il contenuto o fare scraping dei siti web.

## Come fare:

Bash non è la prima scelta per l'analisi dell'HTML, ma può essere fatto con strumenti come `grep`, `awk`, `sed`, o utilità esterne come `lynx`. Per una maggiore robustezza, useremo `xmllint` dal pacchetto `libxml2`.

```bash
# Installa xmllint se necessario
sudo apt-get install libxml2-utils

# Esempio di HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Pagina di Esempio</title>
</head>
<body>
  <h1>Ciao, Bash!</h1>
  <p id="myPara">Bash può leggermi.</p>
</body>
</html>
EOF

# Analizza il Titolo
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Il titolo è: $title"

# Estrai Paragrafo per ID
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Il contenuto del paragrafo è: $para"
```

Output:
```
Il titolo è: Pagina di Esempio
Il contenuto del paragrafo è: Bash può leggermi.
```

## Approfondimento

Anticamente, i programmatori usavano strumenti basati su regex come `grep` per scansionare l'HTML, ma era un metodo goffo. L'HTML non è regolare, è contestuale. Gli strumenti tradizionali non tengono conto di questo e possono essere soggetti a errori.

Alternative? Numerose. Python con Beautiful Soup, PHP con DOMDocument, JavaScript con parser DOM—linguaggi con librerie progettate per comprendere la struttura dell'HTML.

Usare `xmllint` negli script bash è solido per compiti semplici. Capisce l'XML e, per estensione, l'XHTML. L'HTML regolare può essere imprevedibile, però. Non segue sempre le rigorose regole dell'XML. `xmllint` forza l'HTML in un modello XML il che funziona bene per l'HTML ben formato ma può inciampare sulle cose disordinate.

## Vedi Anche

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): Smistifica l'HTML DOM.
- [MDN Web Docs - Analisi e serializzazione XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): Per i principi di analisi XML che si applicano a XHTML.
- [Documentazione di Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Una libreria Python per l'analisi dell'HTML.
- [Documentazione di libxml2](http://xmlsoft.org/): Dettagli su `xmllint` e strumenti XML correlati.
