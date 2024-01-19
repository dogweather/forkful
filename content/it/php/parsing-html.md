---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/parsing-html.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

L'analisi sintattica (parsing) di HTML è il processo di esaminazione del codice HTML per estrarre informazioni specifiche. I programmatori lo fanno per manipolare, estrarre o modificare dati presenti in una pagina web.

## Come si fa:
Ecco un esempio semplice di come fare parsing di un documento HTML utilizzando `DOMDocument` e `DOMXPath` in PHP.

```PHP
<?php
$doc = new DOMDocument();
libxml_use_internal_errors(TRUE); // disable libxml errors
$doc->loadHTML(file_get_contents('https://www.example.com'));

$xpath = new DOMXPath($doc);

$classname = "myClass";
$elements = $xpath->query("//*[contains(concat(' ', normalize-space(@class), ' '), ' $classname ')]");

if (!is_null($elements)) {
  foreach ($elements as $element) {
    echo "<br/>[". $element->nodeName. "]";
    $nodes = $element->childNodes;
    foreach ($nodes as $node) {
      echo $node->nodeValue. "\n";
    }
  }
}
?>
```

Questo script cerca e stampa i nodi che hanno la classe CSS "myClass" nel documento HTML di example.com.

## Approfondimenti:
Storicamente, si usavano delle espressioni regolari per fare il parsing dell'HTML. Però, l'HTML non è un linguaggio regolare, quindi è diventato chiaro che un parser vero e proprio è la scelta migliore.

Tra le alternative ci sono SimpleHTML, che è leggermente più facile da usare, e HTML Purifier, che ha una sicurezza maggiore. Per quanto riguarda la scelta, dipende molto dalle esigenze specifiche del tuo progetto.

Quando si parla di implementazione, `DOMDocument` crea un Document Object Model (DOM) del tuo HTML, facendoti andare avanti e indietro attraverso i nodi per raccogliere i dati. `DOMXPath` è un modo molto potente ed efficace per accedere a questo modello.

## Guarda Anche: 
- "Manipulating HTML and XML documents with cURL and DOM." - https://phpenthusiast.com/blog/manipulate-html-xml-with-php-dom
- "PHP: DOMDocument - Manual." - https://www.php.net/manual/en/class.domdocument.php
- "PHP: DOMXPath - Manual." - https://www.php.net/manual/en/class.domxpath.php
- "PHP Simple HTML DOM Parser." - http://simplehtmldom.sourceforge.net/manual.htm
- "HTML Purifier - Filter your HTML the standards-compliant way!" - http://htmlpurifier.org/