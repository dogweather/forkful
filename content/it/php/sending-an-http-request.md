---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che Cosa e Perché?
Una richiesta HTTP è un messaggio inviato da un client a un server per ottenere dati. Come programmatori, l'utilizziamo per comunicare con gli altri server, scaricare file o interagire con servizi web.

## Come Fare:
Vediamo un semplice esempio usando PHP e cURL per inviare una richiesta GET a un server.

```PHP
<?php
// inizializza
$ch = curl_init();

// imposta l'URL
curl_setopt($ch, CURLOPT_URL, "http://www.example.com");

// esegue
$output = curl_exec($ch);

// chiude
curl_close($ch);

// stampa l'output
echo $output;
?>
```

Quando esegui questo file, vedrai una risposta HTML dal server "www.example.com".

## Approfondimento:
La funzione cURL PHP è stata introdotta nella versione 4.0.2, da allora è uno strumento potente per inviare richieste HTTP. 

Esistono alternative come file_get_contents() o la libreria Guzzle HTTP Client, ma cURL rimane popolare per la sua versatilità e controllo dettagliato della richiesta.

Per inviare dati POST, impostare CURLOPT_POST su true e CURLOPT_POSTFIELDS con il vostro array di dati.

```PHP
curl_setopt($ch, CURLOPT_POST, true);
curl_setopt($ch, CURLOPT_POSTFIELDS, array('key1' => 'value1', 'key2' => 'value2'));
```

## Vedi Anche:
Per un'ulteriore lettura sulle richieste HTTP e su come gestirle con PHP, consulta queste risorse:

cURL Manuale PHP: http://php.net/manual/it/book.curl.php

Libreria Guzzle HTTP Client: http://docs.guzzlephp.org/en/latest/

Funzione PHP file_get_contents(): http://php.net/manual/it/function.file-get-contents.php