---
title:                "PHP: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web è una delle attività più comuni nello sviluppo di un sito o di un'applicazione. È essenziale per ottenere dati e informazioni utili da altre fonti online e incorporarle nel nostro progetto.

## Come fare

Per scaricare una pagina web utilizzando PHP, abbiamo bisogno di una libreria chiamata cURL. Possiamo installarla facilmente utilizzando il gestore di pacchetti composer.

```PHP
// Aggiungiamo la dipendenza al nostro file composer.json
"require": {
    "php": "^7.4",
    "guzzlehttp/guzzle": "^7.0"
}

// Eseguiamo il comando composer install per installare la libreria
composer install
```

Una volta installata la libreria, possiamo codificare la nostra richiesta HTTP utilizzando il metodo GET e ottenere la pagina web che vogliamo scaricare.

```PHP
// Importiamo la libreria cURL e creiamo una nuova istanza
use GuzzleHttp\Client;
$client = new Client();

// Eseguiamo una richiesta GET all'URL della pagina web che vogliamo scaricare
$response = $client->get('https://www.miosito.it');

// Otteniamo il contenuto della pagina
$body = $response->getBody()->getContents();

// Stampiamo il contenuto nella nostra pagina
echo $body;
```

Se vogliamo specificare dei parametri nella nostra richiesta, possiamo farlo utilizzando il metodo `get()` e passando un array con i parametri desiderati.

```PHP
// Eseguiamo una richiesta GET con parametri
$response = $client->get('https://api.miosito.it/users', [
    'query' => [
        'id' => 123,
        'name' => 'Mario'
    ]
]);

// Otteniamo il contenuto della risposta
$body = $response->getBody()->getContents();
```

## Approfondimento

Oltre alle richieste HTTP GET, ci sono altri metodi che possiamo utilizzare per scaricare una pagina web. Ad esempio, possiamo utilizzare una richiesta POST per inviare dati ad una form o una richiesta PUT per aggiornare informazioni su un server.

Possiamo anche utilizzare la libreria cURL per gestire le risposte ottenute, come ad esempio verificare lo stato di una richiesta o ottenere gli header della risposta.

Per ulteriori informazioni e dettagli su come utilizzare la libreria cURL per scaricare una pagina web, si consiglia di consultare la documentazione ufficiale.

## Vedi anche

- [Documentazione ufficiale della libreria cURL](https://www.php.net/manual/en/book.curl.php)
- [Esempi di utilizzo della libreria cURL](https://www.php.net/manual/en/ref.curl.php)
- [Composer - Gestore di pacchetti per PHP](https://getcomposer.org/)