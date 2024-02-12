---
title:                "Inviare una richiesta HTTP con autenticazione di base"
aliases:
- /it/google-apps-script/sending-an-http-request-with-basic-authentication/
date:                  2024-02-01T22:02:21.512292-07:00
model:                 gpt-4-0125-preview
simple_title:         "Inviare una richiesta HTTP con autenticazione di base"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Inviare una richiesta HTTP con autenticazione di base comporta la codifica di un nome utente e di una password in un'intestazione di richiesta per accedere a risorse protette. I programmatori utilizzano questo metodo per l'autenticazione lato server, per integrarsi con API che richiedono l'autenticazione di base per operazioni come il recupero di dati o la pubblicazione di contenuti.

## Come fare:

In Google Apps Script, per inviare una richiesta HTTP con autenticazione di base, si utilizza il servizio `UrlFetchApp` combinato con un'intestazione di autorizzazione codificata in base64. Ecco una guida passo-passo:

1. **Codifica delle Credenziali**: Prima di tutto, codifica il tuo nome utente e la tua password in base64. Google Apps Script non ha una funzione nativa di codifica base64 per le stringhe, quindi userai Utilities.base64Encode per questo scopo.

```javascript
var username = 'IlTuoNomeUtente';
var password = 'LaTuaPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **Imposta le Opzioni della Richiesta**: Con le credenziali codificate pronte, prepara l'oggetto opzioni per la richiesta HTTP, includendo il metodo e le intestazioni.

```javascript
var options = {
  method: 'get', // o 'post', 'put', a seconda delle tue necessità
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // opzioni aggiuntive come 'muteHttpExceptions' per la gestione degli errori possono essere aggiunte qui
};
```

3. **Effettua la Richiesta**: Usa il metodo `UrlFetchApp.fetch` con l'URL di destinazione e l'oggetto opzioni.

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

L'output esemplificativo al seguito della richiesta di successo varierà in base alla risposta dell'API. Per un'API basata su JSON, potresti vedere qualcosa come:

```
{"status":"Successo","data":"Dati della risorsa qui..."}
```

Assicurati di gestire possibili errori HTTP controllando il codice di risposta o utilizzando l'opzione `muteHttpExceptions` per una gestione degli errori più controllata.

## Approfondimento

Inviare una richiesta HTTP con autenticazione di base è stato un metodo standard in molti linguaggi di programmazione per accedere a risorse web che richiedono autenticazione. Nel contesto di Google Apps Script, `UrlFetchApp` offre un modo diretto per eseguire queste richieste HTTP, comprese quelle che richiedono autenticazione. L'inclusione delle credenziali di base nelle intestazioni delle richieste è un metodo semplice ma efficace, ma presenta avvertenze di sicurezza, principalmente perché le credenziali vengono inviate in chiaro, solo codificate in base64, che possono essere facilmente decodificate se intercettate.

Per una maggiore sicurezza, sono consigliate alternative come OAuth 2.0, specialmente quando si tratta di dati o operazioni sensibili. Google Apps Script ha il supporto integrato per OAuth 2.0 con la libreria `OAuth2`, semplificando il processo di autenticazione nei confronti di servizi che supportano questo protocollo.

Nonostante i suoi limiti di sicurezza, l'autenticazione di base rimane ampiamente utilizzata per applicazioni semplici o interne non esposte a Internet in modo esteso. È semplice da implementare, in quanto richiede solo una singola richiesta con intestazioni impostate correttamente, rendendola un'opzione attraente per integrazioni rapide o per API dove metodi di sicurezza più elevati non sono disponibili. Tuttavia, si esorta i programmatori a considerare le implicazioni di sicurezza ed esplorare alternative più sicure quando disponibili.
