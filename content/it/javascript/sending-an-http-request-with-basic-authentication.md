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

## Cosa & Perché?
L'invio di una richiesta HTTP con autenticazione di base è un processo crucial per gli sviluppatori web che desiderano accedere a risorse protette da password. Ciò consente loro di autenticarsi con un server e ottenere accesso ai contenuti desiderati.

## Come si fa:
```Javascript
const username = 'esempio';
const password = 'ciao123';
const url = 'http://www.esempio.com';
const headers = new Headers();
headers.set('Authorization', 'Basic ' + btoa(username + ':' + password));

fetch(url, { headers: headers })
  .then(response => response.text())
  .then(data => console.log(data));
```
In questo semplice esempio, utilizziamo la funzione `fetch()` per inviare una richiesta GET all'URL specificato con autenticazione di base. Per farlo, dobbiamo costruire un oggetto `Headers` e impostare l'header di autorizzazione utilizzando il metodo `set()`. Successivamente, passiamo questo oggetto come opzione nella nostra chiamata `fetch()`. La risposta del server viene quindi elaborata nel secondo `then()` e il risultato viene stampato nella console.

## Approfondimento:
L'autenticazione di base è una tecnica di autenticazione molto semplice utilizzata nel protocollo HTTP. È stato introdotto nella specifica HTTP/1.0 e consiste nell'invio di un nome utente e una password, separati da due punti, codificati in Base64 e inseriti nell'header di autorizzazione della richiesta. Sebbene sia molto comune, è considerata non sicura poiché il nome utente e la password sono trasmessi in chiaro e possono essere facilmente intercettati. Un'alternativa più sicura è l'autenticazione basata su token, che utilizza un token generato dal server per verificare l'identità dell'utente.

## Vedi anche:
- [Autenticazione HTTP di base - MDN Web Docs](https://developer.mozilla.org/it/docs/Web/HTTP/Authentication)
- [HTTP/1.0 - Specifica RFC](https://tools.ietf.org/html/rfc1945)