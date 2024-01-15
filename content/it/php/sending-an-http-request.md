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

## Perché

Se si vuole lavorare con dati esterni, ad esempio ottenere informazioni da un sito web o inviare dati a un server, è necessario inviare una richiesta HTTP. Questa operazione è fondamentale per comunicare tra applicazioni e ottenere i dati necessari per la propria applicazione.

## Come fare

Per inviare una richiesta HTTP in PHP, è possibile utilizzare la funzione `curl_init()` per inizializzare una sessione cURL. Quindi, è possibile impostare i parametri della richiesta, aggiungere eventuali header necessari e infine eseguire la richiesta utilizzando `curl_exec()`. Di seguito un esempio di codice che invia una richiesta GET a Google e stampa il contenuto della pagina nel terminale.

```PHP
$ch = curl_init(); //inizializza una sessione cURL
curl_setopt($ch, CURLOPT_URL, "https://www.google.com");//imposta l'URL del destinatario
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);//imposta l'opzione per ottenere il risultato come stringa
curl_setopt($ch, CURLOPT_HTTPHEADER, array( //imposta gli header per la richiesta
    'Content-Type: application/json'
));
$result = curl_exec($ch); //esegue la richiesta e memorizza il risultato in $result
curl_close($ch); //chiude la sessione cURL
echo $result; //stampa il risultato nel terminale
```

Il risultato di questo codice sarà l'intera pagina HTML di Google.

## Approfondimenti

Oltre alla richiesta GET, è possibile inviare anche richieste POST, PUT e DELETE utilizzando `curl_setopt()`. È inoltre possibile gestire errori e autenticarsi utilizzando `curl_setopt()`. Per ulteriori informazioni sull'utilizzo della funzione di cURL in PHP, è possibile consultare la [documentazione ufficiale di PHP](https://www.php.net/manual/en/book.curl.php).

## Vedi anche

- [Introduzione alle richieste HTTP in PHP](https://www.php.net/manual/en/intro.curl.php)
- [Documentazione ufficiale di cURL in PHP](https://www.php.net/manual/en/book.curl.php)
- [Guida alle richieste HTTP in PHP](https://www.w3schools.com/php/php_http.asp)