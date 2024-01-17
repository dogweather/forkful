---
title:                "Scaricare una pagina web"
html_title:           "PHP: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Scaricare una pagina web significa ottenere il contenuto di una pagina web tramite una richiesta HTTP. I programmatori spesso lo fanno per ottenere dati da un sito web, come informazioni su prodotti, notizie o altre informazioni. Questi dati possono poi essere utilizzati per analisi, integrazioni con altri servizi o per creare applicazioni basate sui dati del sito web.

## Come fare:
```
<?php
$url = "https://www.example.com"; // URL della pagina da scaricare
$ch = curl_init(); // Inizializza una sessione cURL
curl_setopt($ch, CURLOPT_URL, $url); // Imposta l'URL della richiesta
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1); // Imposta il trasferimento dei dati come stringa
$output = curl_exec($ch); // Esegui la richiesta
curl_close($ch); // Chiudi la sessione cURL
echo $output; // Stampa il contenuto della pagina
?>
```
Questo codice utilizza la funzione `curl_exec()` per eseguire una richiesta HTTP all'URL specificato, poi utilizza la funzione `curl_close()` per chiudere la sessione cURL. Infine, viene stampato il contenuto della pagina utilizzando la variabile `$output`.

## Approfondimento:
Scaricare una pagina web è diventato sempre più importante con l'avvento dell'era dei dati, dove l'accesso a informazioni precise e aggiornate è fondamentale per le aziende e gli utenti. Oltre alla funzione `curl_exec()`, esistono anche altre librerie e framework, come Guzzle e Symfony, che possono essere utilizzati per scaricare pagine web con maggiori opzioni e funzionalità. Inoltre, ci sono servizi di scraping che permettono di ottenere dati da siti web in modo automatizzato e organizzato.

## Vedi anche:
- [Documentazione di cURL](https://www.php.net/manual/en/book.curl.php)
- [Guzzle](https://packagist.org/packages/guzzlehttp/guzzle)
- [Symfony HttpClient](https://symfony.com/doc/current/http_client.html)
- [Servizi di scraping come Import.io e Scraping Hub](https://www.import.io/, https://scrapinghub.com/)