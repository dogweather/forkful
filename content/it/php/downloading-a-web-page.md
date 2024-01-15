---
title:                "Scaricare una pagina web."
html_title:           "PHP: Scaricare una pagina web."
simple_title:         "Scaricare una pagina web."
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché 

Scaricare una pagina web è un'attività comune per molte persone coinvolte nello sviluppo di siti web o nella creazione di script automatizzati. È utile per controllare come appare una pagina web su diversi dispositivi o per estrarre informazioni da essa. In questo articolo, vedremo come fare con PHP.

## Come Fare

Per scaricare una pagina web in PHP, è necessario utilizzare la funzione `file_get_contents()` insieme all'URL della pagina che si desidera scaricare. Ecco un esempio di codice:

```PHP
<?php
// URL della pagina web da scaricare
$url = "https://www.example.com";

// Utilizzo della funzione file_get_contents() per scaricare la pagina
$html = file_get_contents($url);

// Stampa del contenuto della pagina web
echo $html;
?>
```

L'output di questo esempio sarà il codice HTML della pagina web. È possibile utilizzare questo codice per visualizzare il contenuto della pagina o per estrarre informazioni specifiche utilizzando funzioni di parsing o regolari espressioni.

## Approfondimento

Oltre alla semplice funzione `file_get_contents()`, PHP offre una vasta gamma di funzioni che possono essere utilizzate per scaricare una pagina web e gestire la risposta del server. Ad esempio, la funzione `curl_exec()` offre più opzioni di configurazione e la possibilità di gestire eventuali errori durante la richiesta. Inoltre, è possibile passare anche parametri nel caso in cui la pagina richiesta richieda una richiesta POST.

Un'altra opzione per scaricare una pagina web è utilizzare la libreria PHP Simple HTML DOM, che facilita l'estrazione di informazioni specifiche dalla pagina web utilizzando selezionatori CSS o XPath.

## Vedi Anche

Ecco alcuni link utili per saperne di più su come scaricare una pagina web con PHP:

- Documentazione ufficiale di PHP: https://www.php.net/manual/en/function.file-get-contents.php
- Tutorial di W3Schools: https://www.w3schools.com/php/func_filesystem_file_get_contents.asp
- Guida di PHP Simple HTML DOM: http://simplehtmldom.sourceforge.net/