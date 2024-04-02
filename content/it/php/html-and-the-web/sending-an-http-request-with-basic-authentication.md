---
date: 2024-01-20 18:02:16.973308-07:00
description: "Invio di una richiesta HTTP con autenticazione di base permette di accedere\
  \ a risorse protette online. I programmatori lo utilizzano per interagire con\u2026"
lastmod: '2024-03-13T22:44:43.516402-06:00'
model: gpt-4-1106-preview
summary: "Invio di una richiesta HTTP con autenticazione di base permette di accedere\
  \ a risorse protette online. I programmatori lo utilizzano per interagire con\u2026"
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## What & Why?
Invio di una richiesta HTTP con autenticazione di base permette di accedere a risorse protette online. I programmatori lo utilizzano per interagire con API che richiedono credenziali per garantire sicurezza e restrizione degli accessi.

## How to:
In PHP, la libreria cURL è un amico fidato per mandare richieste HTTP con autenticazione di base. Ecco come si fa:

```PHP
<?php
// Inizializza sessione cURL
$ch = curl_init();

// Imposta URL e opzioni
curl_setopt($ch, CURLOPT_URL, "https://example.com/api/data");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
curl_setopt($ch, CURLOPT_USERPWD, "username:password"); // Sostituire con credenziali reali

// Esegui la richiesta e chiudi
$response = curl_exec($ch);
curl_close($ch);

// Visualizza il risultato
echo $response;
?>
```

Se tutto va per il verso giusto, vedrai i dati restituiti dall'API.

## Deep Dive
L'autenticazione di base HTTP è semplice ma non la più sicura; l'uso in produzione richiede HTTPS. Storicamente, è una delle prime forme di autenticazione HTTP.

Esistono alternative più sicure come OAuth, ma l'autenticazione di base rimane popolare per la sua facilità di implementazione. Si tratta semplicemente di inviare `username:password` codificati in Base64 nell'header della richiesta.

Per capire meglio il processo interno, quando usi cURL in PHP, stai configurando una comunicazione a basso livello con il server remoto. Il tuo PHP non fa altro che compilare la tua richiesta HTTP correttamente, delegando il lavoro sporco a libcurl, il motore sotto cURL.

## See Also
- Documentazione ufficiale di cURL in PHP: https://www.php.net/manual/en/book.curl.php
- Guida sicurezza autenticazione HTTP: https://owasp.org/www-community/controls/Basic_Authentication
- Documentazione ufficiale su autenticazione HTTP in PHP: https://www.php.net/manual/en/features.http-auth.php
