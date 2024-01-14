---
title:                "PHP: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Lo scopo principale di inviare una richiesta HTTP è comunicare con un server web per ottenere informazioni o eseguire un'azione. Questo può essere fatto tramite un browser web o da un'applicazione web utilizzando codice PHP.

## Come fare

Per inviare una richiesta HTTP in PHP, è necessario utilizzare la funzione `file_get_contents()` oppure la classe `CURL`. Ecco un esempio di entrambi i metodi:

```PHP
// Utilizzando file_get_contents()
$response = file_get_contents('https://www.example.com/api/users');

// Utilizzando CURL
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'https://www.example.com/api/users');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$response = curl_exec($ch);
curl_close($ch);

echo $response; // stampa il contenuto della risposta
```

Entrambe le opzioni richiedono un URL come parametro e restituiscono il contenuto della risposta del server. Se si desidera inviare una richiesta specificando dei parametri, si può farlo utilizzando il parametro opzionale `options` nella funzione `file_get_contents()` o utilizzando i metodi di `CURL` come `curl_setopt()`.

## Approfondimento

Quando si invia una richiesta HTTP, è importante comprendere i diversi metodi che si possono utilizzare. I più comuni sono `GET`, `POST`, `PUT` e `DELETE`, ognuno dei quali ha uno scopo e un'utilizzazione specifica. Inoltre, è importante gestire gli errori durante l'invio della richiesta e nel trattare la risposta del server.

## Vedi anche

- [Documentazione ufficiale di PHP sull'invio di richieste HTTP](https://www.php.net/manual/en/function.file-get-contents.php)
- [Tutorial di CURL su TutorialsPoint](https://www.tutorialspoint.com/php/php_and_ajax.htm)
- [Informazioni su codificare e decodificare i dati nelle richieste HTTP](https://www.w3schools.com/php/php_ajax_php.asp)