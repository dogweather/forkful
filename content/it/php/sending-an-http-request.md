---
title:                "Inviare una richiesta http"
html_title:           "PHP: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che cos'è e perché si invia una richiesta HTTP?

In poche parole, inviare una richiesta HTTP significa comunicare con altri sistemi tramite il protocollo HTTP (Hypertext Transfer Protocol). Questo permette ai programmatori di accedere e recuperare informazioni da server esterni, come ad esempio dati da un database o contenuti da un sito web.

I programmatori inviano richieste HTTP perché è un modo efficiente e standardizzato di ottenere dati da altre fonti. In questo modo, possono integrare facilmente le informazioni di cui hanno bisogno nel loro codice e fornire una migliore esperienza utente.

## Come fare:

Ecco un esempio di codice PHP per inviare una richiesta HTTP utilizzando la funzione built-in `file_get_contents()`:

```PHP
$api_url = "http://www.example.com/api/users";
$response = file_get_contents($api_url);
```

In questo esempio, stiamo inviando una richiesta GET all'URL dell'API degli utenti. La risposta viene salvata nella variabile `$response`, che può quindi essere elaborata ulteriormente.

Un altro modo di inviare una richiesta HTTP è utilizzando la libreria cURL integrata in PHP. Di seguito è riportato un esempio di codice che invia una richiesta POST con alcuni dati di esempio al server:

```PHP
$curl = curl_init();

curl_setopt_array($curl, array(
  CURLOPT_URL => "http://www.example.com/api/new_user",
  CURLOPT_RETURNTRANSFER => true,
  CURLOPT_POST => true,
  CURLOPT_POSTFIELDS => array(
    'name' => 'John Doe',
    'email' => 'john.doe@example.com',
    'password' => 'example123'
  )
));

$response = curl_exec($curl);
curl_close($curl);
```

## Approfondimento:

Il protocollo HTTP è stato inventato negli anni '90 da Tim Berners-Lee come parte del World Wide Web. Essenzialmente, è un sistema che consente ai client (browser) e ai server web di comunicare tra loro.

Oltre alle funzioni built-in di PHP come `file_get_contents()` e cURL, ci sono altre librerie e framework che possono essere utilizzati per inviare richieste HTTP, come ad esempio Guzzle o Symfony's HttpClient.

L'implementazione delle richieste HTTP in PHP può variare in base alla versione del linguaggio e alle configurazioni del server. È importante assicurarsi di utilizzare le funzioni e le impostazioni appropriate per il proprio progetto.

## Vedi anche:

- [Funzione PHP `file_get_contents()`](https://www.php.net/manual/en/function.file-get-contents.php)
- [Libreria cURL per PHP](https://www.php.net/manual/en/book.curl.php)
- [Guzzle, una moderna libreria PHP per richieste HTTP](https://docs.guzzlephp.org/)
- [Symfony HttpClient, una libreria basata su Guzzle per gestire richieste HTTP](https://symfony.com/doc/current/http_client.html)