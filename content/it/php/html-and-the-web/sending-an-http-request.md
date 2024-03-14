---
date: 2024-01-20 18:00:10.630040-07:00
description: "Mandare una richiesta HTTP \xE8 come inviare una lettera al web per\
  \ prendere o mandare informazioni. I programmatori lo fanno per interagire con API,\
  \ servizi\u2026"
lastmod: '2024-03-13T22:44:43.513477-06:00'
model: gpt-4-1106-preview
summary: "Mandare una richiesta HTTP \xE8 come inviare una lettera al web per prendere\
  \ o mandare informazioni. I programmatori lo fanno per interagire con API, servizi\u2026"
title: Inviare una richiesta http
---

{{< edit_this_page >}}

## What & Why?
Mandare una richiesta HTTP è come inviare una lettera al web per prendere o mandare informazioni. I programmatori lo fanno per interagire con API, servizi esterni e per scaricare dati.

## How to:
PHP con `curl` è un modo popolare per inviare richieste HTTP. Ecco un esempio per fare una richiesta GET a un API che restituisce dati JSON:

```PHP
<?php
$curl = curl_init();

curl_setopt($curl, CURLOPT_URL, "https://api.exemplo.com/dati");
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);

$risposta = curl_exec($curl);
if ($err = curl_error($curl)) {
    echo 'Errore cURL: ' . $err;
} else {
    echo 'Risposta: ' . $risposta;
}

curl_close($curl);
?>
```

Output d'esempio:
```
Risposta: {"nome":"Giovanni","cognome":"Rossi"}
```

## Deep Dive:
L'uso di `curl` in PHP per inviare richieste HTTP si è evoluto negli anni. Prima, usavamo `fsockopen()` o `file_get_contents()`, ma `curl` offre più controllo e opzioni.

Alternative includono:
- file_get_contents(): semplice per richieste GET.
- fsockopen(): per chi vuole controllare manualmente ogni parte della richiesta HTTP.
- Guzzle: una libreria PHP moderna per client HTTP.

Dettagli sull'implementazione con `curl`:
- `CURLOPT_RETURNTRANSFER`: restituisce la risposta come stringa.
- Gestione errori: `curl_error()` per sapere se qualcosa è andato storto.
- `CURLOPT_HTTPHEADER`: per mandare intestazioni HTTP personalizzate.

## See Also:
- [PHP cURL Manual](https://www.php.net/manual/en/book.curl.php) - Documentazione ufficiale di PHP su cURL.
- [Guzzle](http://docs.guzzlephp.org/en/stable/) - Documentazione della libreria Guzzle.
- [HTTP.cat](https://http.cat/) - Un modo creativo per imparare gli status code HTTP attraverso immagini di gatti.
