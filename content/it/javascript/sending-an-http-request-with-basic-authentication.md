---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Javascript: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

Se stai costruendo un'applicazione web che richiede l'autenticazione di base per accedere a determinate risorse, dovrai capire come inviare una richiesta HTTP con l'autenticazione di base. Questa è una tecnica comune per proteggere le informazioni sensibili su una rete.

## Come Fare

Per inviare una richiesta HTTP con l'autenticazione di base in Javascript, dovrai utilizzare la funzione `fetch()` con il parametro `headers` per specificare le credenziali dell'utente. Ecco un esempio di codice:

```
let url = "http://www.example.com/api/resource"
let username = "myUsername"
let password = "myPassword"

fetch(url, {
  method: 'GET',
  headers: {
    'Authorization': 'Basic ' + btoa(username + ":" + password)
  }
})
.then(response => {
  return response.json();
})
.then(data => {
  console.log(data); // output: dati della risorsa
})
.catch(error => {
  console.error(error);
})
```

In questo esempio, stiamo inviando una richiesta GET all'endpoint "http://www.example.com/api/resource" con le credenziali specificate. Nota che usiamo `btoa()` per codificare le credenziali in base64 prima di includerle nell'header di autorizzazione.

## Approfondimento

Quando si invia una richiesta HTTP con autenticazione di base, è importante comprendere alcuni concetti chiave:

- Le credenziali devono essere codificate in base64 per essere incluse nell'header di autorizzazione.
- Le credenziali inviate in una richiesta HTTP non sono crittografate, quindi è importante utilizzare HTTPS per proteggere la comunicazione.
- Per decodificare le credenziali in base64, puoi utilizzare la funzione `atob()` in Javascript.
- Se si riceve un codice di errore 401 (non autorizzato) in risposta alla richiesta, è probabile che le credenziali fornite siano errate.

## Vedi Anche

- [Documentazione ufficiale di MDN su `fetch()`](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Tutorial su autenticazione di base in Javascript](https://www.sitepoint.com/basic-http-authentication-node-js/)
- [Articolo sulle best practices per la sicurezza delle API](https://www.owasp.org/index.php/REST_Security_Cheat_Sheet#Communications)