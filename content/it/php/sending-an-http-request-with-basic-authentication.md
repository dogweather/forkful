---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "PHP: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché
Molti siti web richiedono l'autenticazione per accedere al loro contenuto, quindi imparare come inviare una richiesta HTTP con autenticazione di base è fondamentale per accedere a queste risorse.

## Come
Per inviare una richiesta HTTP con autenticazione di base in PHP, possiamo utilizzare la funzione `curl_init()` e passare il parametro `CURLOPT_USERPWD` con le credenziali desiderate. Di seguito è riportato un esempio di codice che invia una richiesta GET a un'API protetta da autenticazione di base:

```PHP
<?php
// Inizializzazione di cURL
$ch = curl_init();
// Impostazione dell'URL di destinazione
curl_setopt($ch, CURLOPT_URL, 'https://www.example.com/api');
// Impostazione dell'autenticazione di base
curl_setopt($ch, CURLOPT_USERPWD, 'username:password');
// Impostazione della richiesta come GET
curl_setopt($ch, CURLOPT_HTTPGET, true);
// Esecuzione della richiesta
$response = curl_exec($ch);

// Stampa dell'output della richiesta
echo $response;

// Chiusura della sessione cURL
curl_close($ch);
```

In questo esempio, abbiamo specificato il nome utente e la password desiderati come stringa nel formato `username:password`. Possiamo anche passare un array con il parametro `CURLOPT_USERPWD` se necessario.

## Deep Dive
Quando inviamo una richiesta HTTP con autenticazione di base, le credenziali vengono codificate in base64 prima di essere inviate al server. Questo processo di codifica rende le credenziali meno leggibili in caso di intercettazione della richiesta. Inoltre, è consigliabile utilizzare HTTPS per cifrare la comunicazione e proteggere ulteriormente le credenziali.

## Vedere Anche
- [Documentazione PHP su cURL](https://www.php.net/manual/en/book.curl.php)
- [Documentazione cURL su CURLOPT_USERPWD](https://curl.se/libcurl/c/CURLOPT_USERPWD.html)
- [Tutorial su autenticazione di base con cURL in PHP](https://www.interserver.net/tips/kb/use-curl-php/)