---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Gleam: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
In questo articolo, parleremo di come inviare una richiesta HTTP con l'autenticazione di base utilizzando il linguaggio di programmazione Gleam. Ma prima di addentrarci nei dettagli, vediamo brevemente di cosa si tratta e perché i programmatori lo fanno. In poche parole, l'autenticazione di base è un metodo di sicurezza che richiede un nome utente e una password per accedere a determinate informazioni o servizi su Internet. I programmatori spesso utilizzano questo metodo per proteggere i dati sensibili e garantire l'accesso solo agli utenti autorizzati.

## Come fare:
Per inviare una richiesta HTTP con l'autenticazione di base, dobbiamo prima importare la libreria http in Gleam. Quindi possiamo utilizzare la funzione `basic_auth_request`, specificando il metodo di richiesta, l'URL e le credenziali di autenticazione come argomenti. Di seguito è riportato un esempio di codice Gleam che mostra come inviare una richiesta GET con l'autenticazione di base:

```Gleam
import http

basic_auth_request("GET", "https://api.example.com", "username", "password")
```
L'output di questa richiesta sarà una risposta con i dati richiesti o un codice di errore, se l'autenticazione fallisce.

## Approfondimento:
L'autenticazione di base è stata introdotta per la prima volta nel protocollo HTTP nel 1999 come un modo semplice per autenticare gli utenti. Sebbene sia ancora ampiamente utilizzata, è stata criticata per la sua mancanza di sicurezza e la mancanza di supporto per la gestione delle sessioni. Per questo motivo, molte alternative più sicure sono state sviluppate, come l'autenticazione digest e l'autenticazione a chiave pubblica.

Per implementare l'autenticazione di base in Gleam, la libreria http utilizza il campo di intestazione HTTP "Authorization" per includere le credenziali di autenticazione nella richiesta. Le informazioni di autenticazione sono codificate in Base64 per garantire la riservatezza durante il trasferimento dei dati.

## Vedi anche:
Se vuoi approfondire l'argomento o scoprire altre alternative all'autenticazione di base, ecco alcuni link utili:

- Documentazione Gleam sull'utilizzo della libreria http: https://gleam.run/modules/http.html
- Articolo sulle differenze tra autenticazione di base e autenticazione digest: https://www.mishpati.co.il/using-http-basic-over-https/
- Spiegazione dettagliata su come funziona l'autenticazione di base: https://www.httpwatch.com/httpgallery/authentication/#basicbasic
- Documentazione ufficiale del protocollo HTTP che spiega l'utilizzo dell'intestazione "Authorization": https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.8

Con queste risorse, sei pronto per utilizzare l'autenticazione di base nelle tue applicazioni Gleam!