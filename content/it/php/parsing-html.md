---
title:                "Parsing html"
html_title:           "PHP: Parsing html"
simple_title:         "Parsing html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/parsing-html.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Il parsing HTML è il processo di analisi del codice HTML per estrarre informazioni specifiche come tag, attributi e contenuti. I programmatori lo fanno per manipolare e utilizzare i dati raccolti per scopi come il web scraping e l'estrazione di dati.

## Come fare:
Ecco un esempio di codice PHP per effettuare il parsing di una pagina HTML e stampare il contenuto di un tag specifico utilizzando la libreria DOMDocument:

```
// Creazione di un'istanza DOMDocument
$dom = new DOMDocument();
// Caricamento della pagina HTML da analizzare
$dom->loadHTMLFile("pagina.html");
// Ricerca del tag desiderato
$tag = $dom->getElementsByTagName("p")->item(0);
// Stampa del contenuto del primo paragrafo
echo $tag->textContent;
```

Questo codice utilizzerà la libreria DOMDocument per caricare la pagina specificata e ricercare il primo tag "p", quindi stampare il suo contenuto utilizzando la proprietà "textContent".

## Approfondimenti:
Il parsing HTML è stato introdotto per la prima volta nel 1993 con la specifica HTML 2.0. Ci sono diversi modi per analizzare il codice HTML, tra cui l'utilizzo di librerie come Simple HTML DOM e la creazione di espressioni regolari personalizzate.

## Vedi anche:
- [Libreria DOMDocument di PHP] (https://www.php.net/manual/en/class.domdocument.php)
- [Simple HTML DOM] (https://simplehtmldom.sourceforge.io/)
- [Tutorial di parsing HTML con PHP] (https://www.w3schools.com/php/php_ajax_html_xml.asp)