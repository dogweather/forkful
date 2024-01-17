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

## Cos'è e Perché?

Invio di una richiesta HTTP con autenticazione di base è un modo per accedere a una risorsa protetta su un server utilizzando un nome utente e una password. I programmatori spesso utilizzano questo metodo per accedere a risorse come API o pagine web che richiedono l'autenticazione prima del loro utilizzo.

## Come fare:

Ecco un esempio di codice PHP che invia una richiesta HTTP con autenticazione di base e stampa la risposta ricevuta:

```
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'www.example.com');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, 'username:password');
$output = curl_exec($ch);
echo $output;
```

Output:
```
<html>
<head>
<title>Hello World!</title>
</head>
<body>
<p>This is a protected resource.</p>
</body>
</html>
```

## Approfondimenti:

La comunicazione HTTP tra un client e un server è guidata dallo scambio di richieste e risposte. Nell'inviare una richiesta con autenticazione di base, il client codifica il nome utente e la password in base64 e li invia nel campo di intestazione "Authorization". L'alternativa a questo metodo è l'autenticazione tramite token, che richiede un token di accesso invece di un nome utente e una password.

Per implementare l'autenticazione di base in una richiesta HTTP, è necessario utilizzare una libreria come curl o guzzle. Inoltre, è importante ricordare di utilizzare una connessione sicura tramite HTTPS per proteggere le credenziali di autenticazione durante la trasmissione.

## Vedi anche:

- Documentazione ufficiale di PHP su come inviare richieste HTTP con autenticazione di base: https://www.php.net/manual/it/features.http-auth.php
- Articolo su Medium che spiega come utilizzare l'autenticazione di base in richieste API in PHP: https://medium.com/@mrjimbot/how-to-do-basic-authentication-with-php-3c9ba5db79d
- Esempi di codice su GitHub che mostrano l'utilizzo di autenticazione di base in diverse situazioni: https://github.com/search?q=basic+authentication+php&type=Repositories