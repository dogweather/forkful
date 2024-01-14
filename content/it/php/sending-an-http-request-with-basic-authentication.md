---
title:                "PHP: Inviare una richiesta Http con autenticazione di base"
simple_title:         "Inviare una richiesta Http con autenticazione di base"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Perché inviare una richiesta HTTP con autenticazione di base

L'autenticazione di base è un popolare metodo di autenticazione per le richieste HTTP. Ciò significa che per accedere a un'API o a una pagina web protetta, l'utente deve fornire le proprie credenziali, ovvero un nome utente e una password. Ma perché dovremmo utilizzare questo metodo specifico invece di altri?

## Come fare

Per inviare una richiesta HTTP con autenticazione di base in PHP, possiamo utilizzare la funzione `curl_init()` per iniziare una sessione cURL. Quindi, dobbiamo impostare l'opzione `CURLOPT_HTTPHEADER` con le informazioni di autenticazione richieste nel formato corretto. Infine, possiamo eseguire la richiesta con la funzione `curl_exec()`.

```PHP
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "https://esempio.com/api/richiesta");
curl_setopt($ch, CURLOPT_HTTPHEADER, array('Authorization: Basic ' . base64_encode("nome_utente:password")));
$result = curl_exec($ch);
curl_close($ch);
```

L'esempio sopra mostra come inviare una richiesta POST all'endpoint `https://esempio.com/api/richiesta` utilizzando l'autenticazione di base. Assicurati di sostituire "nome_utente" e "password" con le tue credenziali.

Il risultato di questa richiesta dipenderà dall'API o dalla pagina web a cui stai accedendo. Se le tue credenziali sono corrette, dovresti ricevere una risposta con un codice di stato 200 OK e i dati richiesti.

## Approfondimento

Sebbene l'autenticazione di base sia uno dei metodi più semplici per l'autenticazione delle richieste HTTP, può non essere la scelta migliore in tutti i casi. Se stai lavorando con dati sensibili o altamente riservati, potresti voler utilizzare metodi di autenticazione più avanzati, come OAuth o JWT.

Inoltre, l'autenticazione di base non crittografa i dati inviati come parte della richiesta, quindi può essere vulnerabile in determinate situazioni. Per questo motivo, è importante utilizzare sempre una connessione HTTPS sicura quando si inviano richieste con autenticazione di base.

# Vedi anche

- [Documentazione PHP su autenticazione di base con cURL](https://www.php.net/manual/en/function.curl-setopt.php#example-4404)
- [Approfondimenti sull'autenticazione HTTP](https://www.ietf.org/rfc/rfc2617.txt)
- [Esempio pratico di autenticazione di base con cURL e PHP](https://www.techpaa.com/2017/03/php-curl-basic-authentication-rest-api-tutorial.html)