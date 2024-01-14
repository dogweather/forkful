---
title:                "Javascript: Invio di una richiesta http con autenticazione di base"
simple_title:         "Invio di una richiesta http con autenticazione di base"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Perché

L'invio di richieste HTTP con autenticazione di base è un'operazione comune nella programmazione front-end e back-end. È utile per accedere a risorse protette o per verificare l'identità dell'utente prima di permettere l'accesso a determinate funzionalità di un sito web o applicazione. In questo post, impareremo come inviare una richiesta HTTP con autenticazione di base utilizzando Javascript.

## Come

Per inviare una richiesta HTTP con autenticazione di base, è necessario utilizzare l'oggetto XMLHttpRequest in Javascript. Qui di seguito è riportato un esempio di codice:

```Javascript
var xhr = new XMLHttpRequest();
xhr.open('GET', 'url_risorsa', true);
xhr.setRequestHeader("Authorization", "Basic " + btoa(username + ":" + password));
xhr.send();
```

Nel codice sopra, abbiamo creato un oggetto XMLHttpRequest, specificato l'URL della risorsa da accedere e impostato l'intestazione di autorizzazione utilizzando il metodo `setRequestHeader()`. L'intestazione di autorizzazione richiede che venga fornita una stringa codificata in base64 contenente il nome utente e la password separati da due punti.

Una volta inviata la richiesta, possiamo recuperare la risposta utilizzando il seguente codice:

```Javascript
xhr.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(xhr.responseText);
  }
};
```

Il codice sopra esegue una funzione quando la richiesta è completata e restituisce uno status 200 (OK). Utilizzando il metodo `responseText`, possiamo accedere al contenuto della risposta.

## Deep Dive

Inviare una richiesta HTTP con autenticazione di base è un'operazione relativamente semplice, ma ci sono alcune cose importanti da tenere a mente:

- Assicurarsi che l'URL della risorsa sia corretto e che la richiesta sia inviata utilizzando il metodo corretto (ad esempio GET, POST, PUT, DELETE).
- L'utilizzo di una stringa codificata in base64 per l'autenticazione di base non è completamente sicuro, ma è comunque comune nelle richieste HTTP.
- È possibile utilizzare diversi metodi per codificare in base64 una stringa, ad esempio `window.btoa()` e `Buffer.from()`.
- Per una maggiore sicurezza, è consigliabile utilizzare l'autenticazione con token invece dell'autenticazione di base.

## Vedi anche

Ecco alcuni link utili per saperne di più sulle richieste HTTP con autenticazione di base:

- Documentazione su XMLHttpRequest: https://developer.mozilla.org/it/docs/Web/API/XMLHttpRequest
- Codifica in base64 con Javascript: https://developer.mozilla.org/it/docs/Web/API/WindowBase64/btoa
Se vogliono ulteriori informazioni riguardanti i token di autenticazione, cliccare qui: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#ation_Basic

Happy coding!