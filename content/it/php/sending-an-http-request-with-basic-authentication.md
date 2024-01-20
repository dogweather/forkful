---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

L'invio di una richiesta HTTP con autenticazione di base (Basic Auth) è un metodo di accesso a risorse web protette. I programmatori lo fanno perché consente alle applicazioni di fornire credenziali utente in una richiesta HTTP per autenticare l'accesso a una risorsa.

## Come Fare:

Per inviare una richiesta HTTP con autenticazione di base in PHP, si può utilizzare la libreria cURL. Ecco un semplice esempio:

```PHP
<?php
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, "http://example.com/resource");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "username:password");

$output = curl_exec($ch);

curl_close($ch);      
?>
```

Nel codice precedente, `username:password` sono le credenziali di autenticazione.

## Approfondimento

L'autenticazione di base HTTP è uno dei modi più antichi per autenticare gli utenti, introdotto con HTTP/1.0 nel 1996. Sebbene sia ancora in uso, ha dei limiti, come il fatto che le credenziali sono trasmesse come testo in chiaro.

Esistono alternative più sicure, come l'autenticazione Digest e l'autenticazione basata su token, che cifra o maschera le credenziali. Inoltre, potresti voler considerare l'autenticazione OAuth se stai sviluppando un'applicazione web interattiva che necessita di autorizzazioni complesse.

Dettagli implementativi: L'autenticazione di base utilizza l'intestazione `Authorization` per trasportare le credenziali. Il formato è `Authorization: Basic {cred}`, dove `{cred}` è la stringa `username:password` codificata in base64.

## Vedi Anche

- [Documentazione PHP cURL](https://www.php.net/manual/en/book.curl.php)
- [RFC 7617 - Autenticazione HTTP Basic](https://tools.ietf.org/html/rfc7617)
- [Tutorial Auenticazione Digest](https://www.tutorialspoint.com/php/php_http_digest_authentication.htm)
- [Tutorial Autenticazione basata su token in PHP](https://www.cloudways.com/blog/rest-api-in-php-and-token-based-authentication)
- [Guida OAuth 2.0](https://oauth.net/getting-started/)