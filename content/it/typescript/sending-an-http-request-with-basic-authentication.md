---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "TypeScript: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Invio di una richiesta HTTP con autenticazione di base in TypeScript

## Cos'è e perché farlo?

L'invio di una richiesta HTTP con autenticazione di base è un modo per garantire la sicurezza e l'accesso controllato ad un server web. Questo tipo di autenticazione richiede l'inclusione di credenziali di accesso nella richiesta, solitamente un nome utente e una password. I programmatori utilizzano questo metodo per verificare l'identità dell'utente e consentire l'accesso ai dati protetti sul server.

## Come fare:

Per inviare una richiesta HTTP con autenticazione di base in TypeScript, è necessario innanzitutto specificare l'URL del server a cui si sta facendo la richiesta. È possibile farlo utilizzando la funzione `fetch` di TypeScript, che prende come primo argomento l'URL e restituisce una promessa contenente la risposta del server. 

Ecco un esempio di codice che invia una richiesta ad un server con autenticazione di base e stampa la risposta:

```TypeScript
let serverURL = "https://example.com/api/data";
let username = "utente";
let password = "password";

fetch(serverURL, { 
    headers: {
        "Authorization" : "Basic " + btoa(username + ":" + password)
    }
})
.then(response => {
    console.log(response);
});
```

La funzione `btoa` viene utilizzata per codificare le credenziali in base64, come richiesto dall'autenticazione di base.

## Approfondimenti:

In passato, l'autenticazione di base era uno dei metodi più comuni per proteggere i server web, ma oggi è considerata meno sicura rispetto ad altri metodi come l'autenticazione a due fattori o l'utilizzo di protocolli di sicurezza come TLS. Tuttavia, può ancora essere una buona opzione per applicazioni interne o per i casi in cui la sicurezza non è una grande preoccupazione.

Se si prevede di utilizzare l'autenticazione di base, è importante prendere in considerazione le implicazioni di sicurezza e gli eventuali rischi associati. Inoltre, esistono anche altri metodi di autenticazione che potrebbero essere più adatti per la vostra applicazione.

È importante anche capire come il server gestisce le richieste con autenticazione di base e cosa accade se le credenziali non sono valide. In caso di problemi, il server restituirà uno stato HTTP 401 (Non autorizzato) che indica che le credenziali non sono state accettate.

## Vedi anche:

Per ulteriori informazioni su come utilizzare TypeScript per gestire le richieste HTTP, puoi consultare la documentazione ufficiale di TypeScript sulla funzione `fetch` e sull'API di rete. Inoltre, puoi esplorare altri metodi di autenticazione e come gestire la sicurezza delle tue applicazioni web.