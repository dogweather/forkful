---
title:                "PHP: Analisi di html"
simple_title:         "Analisi di html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si lavora con i dati tratti da pagine web, è necessario estrarre solo alcune parti specifiche del codice HTML per poterle elaborare. Questo processo è chiamato "parsing" e può essere fatto in diversi modi, tra cui utilizzando il linguaggio di programmazione PHP. In questo articolo, esploreremo perché è importante saper fare il parsing dell'HTML e come farlo utilizzando PHP.

## Come Fare

Per prima cosa, dovrai avere una conoscenza di base di PHP e delle sue funzioni per poter eseguire il parsing dell'HTML. Ci sono diverse librerie PHP disponibili per il parsing dell'HTML, tra cui DOMDocument e SimpleHTMLDom. Utilizzeremo SimpleHTMLDom in questo esempio.

Iniziando, dobbiamo scaricare e includere la libreria SimpleHTMLDom nel nostro script PHP. Una volta fatto ciò, dobbiamo creare una variabile che conterrà il nostro codice HTML come una stringa.

```PHP
<?php
include('simple_html_dom.php');

$codiceHTML = "
<html>
  <head>
    <title>Il mio Blog di PHP</title>
  </head>
  <body>
    <h1>Ciao a tutti!</h1>
    <p>Bentornati nel mio blog di PHP.</p>
    <div class='post'>
      <h2>Imparare il parsing di HTML</h2>
      <p>In questo post, esploreremo come fare il parsing di HTML utilizzando PHP.</p>
    </div>
  </body>
</html>
";
```

Una volta che abbiamo il nostro codice HTML in una variabile, possiamo utilizzare la funzione `str_get_html()` di SimpleHTMLDom per convertirlo in un oggetto DOM che possiamo manipolare.

```PHP
<?php
$dom = str_get_html($codiceHTML);
```

Ora possiamo utilizzare i metodi di SimpleHTMLDom per estrarre le parti specifiche del codice HTML che ci interessano. Ad esempio, se volessimo ottenere il titolo della nostra pagina, useremmo il metodo `find()` per cercare l'elemento `<title>` e poi il metodo `plaintext` per ottenere il suo contenuto.

```PHP
<?php
$titolo = $dom->find('title', 0)->plaintext;
echo $titolo; //output: Il mio Blog di PHP
```

Inoltre, se volessimo ottenere il contenuto della nostra `<div>` con il class "post", useremmo il metodo `find()` con il selettore CSS `.post` e poi il metodo `plaintext` per ottenere il suo contenuto.

```PHP
<?php
$contenuto = $dom->find('.post', 0)->plaintext;
echo $contenuto; //output: Imparare il parsing di HTML In questo post, esploreremo come fare il parsing di HTML utilizzando PHP.
```

Ci sono molti altri metodi disponibili in SimpleHTMLDom per estrarre parti specifiche di codice HTML. Assicurati di consultare la documentazione ufficiale per maggiori informazioni.

## Deep Dive

Ora che hai una comprensione di base di come fare il parsing dell'HTML con PHP, vediamo alcune altre cose importanti da sapere.

- Quando si effettua il parsing dell'HTML, è importante avere una buona conoscenza di come funziona la struttura del codice HTML. Questo ti aiuterà a capire quali elementi e attributi selezionare per estrarre le informazioni desiderate.
- Se stai eseguendo il parsing di pagine web esterne, assicurati di rispettare le politiche di scraping etico e di consultare i termini di utilizzo del sito web.
- Per rendere il tuo codice più efficiente, puoi utilizzare le espressioni regolari per estrarre parti specifiche di codice HTML invece di utilizzare librerie esterne.

## Vedi Anche

- Documentazione SimpleHTMLDom: https://simplehtmldom.sourceforge.io/
- Manipolazione HTML con PHP: https://www.php.net/manual/en/book.dom.php
- Guida alle espressioni regolari in PHP: https://www.php.net/manual/en/reference.pcre.pattern.syntax.php