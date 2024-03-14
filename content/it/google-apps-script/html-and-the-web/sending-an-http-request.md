---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:26.713030-07:00
description: "L'invio di una richiesta HTTP in Google Apps Script consiste nel fare\
  \ una chiamata programmatica a un server web esterno o a un'API. I programmatori\
  \ fanno\u2026"
lastmod: '2024-03-13T22:44:42.951351-06:00'
model: gpt-4-0125-preview
summary: "L'invio di una richiesta HTTP in Google Apps Script consiste nel fare una\
  \ chiamata programmatica a un server web esterno o a un'API. I programmatori fanno\u2026"
title: Inviare una richiesta HTTP
---

{{< edit_this_page >}}

## Cosa e Perché?

L'invio di una richiesta HTTP in Google Apps Script consiste nel fare una chiamata programmatica a un server web esterno o a un'API. I programmatori fanno ciò per recuperare o inviare dati a servizi web, integrando un vasto regno di risorse e funzionalità web direttamente nei loro progetti Google Apps Script.

## Come fare:

In Google Apps Script, il modo primario per inviare una richiesta HTTP è utilizzare il servizio `UrlFetchApp`. Questo servizio fornisce metodi per effettuare richieste HTTP GET e POST. Ecco un semplice esempio di come effettuare una richiesta GET per recuperare dati JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Per una richiesta POST, comunemente usata per inviare dati a un server, è necessario includere più dettagli nel parametro delle opzioni:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    chiave1: 'valore1',
    chiave2: 'valore2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Convertire l'oggetto JavaScript in una stringa JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Questi frammenti mostrano implementazioni di base delle richieste GET e POST. L'output dipenderà dalla risposta dell'API e può essere visualizzato nel Logger di Google Apps Script.

## Approfondimento

Il servizio `UrlFetchApp` di Google Apps Script si è evoluto significativamente dalla sua nascita, offrendo un controllo più sfumato sulle richieste HTTP con funzionalità come l'impostazione degli header, del payload e la gestione di multipart/form-data per il caricamento di file. Sebbene fornisca un mezzo semplice per integrare servizi web esterni, gli sviluppatori che provengono da linguaggi backend più robusti possono trovare le sue funzionalità un po' limitanti rispetto a librerie come il `requests` di Python o l'API `fetch` di JavaScript in Node.js.

Una limitazione degna di nota è il limite di tempo di esecuzione per Google Apps Script, che influisce sulle richieste di lunga durata. Inoltre, mentre `UrlFetchApp` copre un'ampia gamma di casi d'uso, scenari più complessi che coinvolgono l'autenticazione OAuth o la gestione di payload molto grandi possono richiedere soluzioni creative o l'utilizzo di risorse aggiuntive di Google Cloud.

Tuttavia, per la maggior parte delle integrazioni che gli sviluppatori di Google Workspace incontrano - che vanno dall'automazione del recupero dati alla pubblicazione di aggiornamenti su servizi esterni - `UrlFetchApp` fornisce uno strumento potente e accessibile. La sua integrazione in Google Apps Script significa che non c'è bisogno di librerie esterne o di configurazioni complesse, rendendo le richieste HTTP relativamente semplici da eseguire all'interno dei vincoli di Google Apps Script. Man mano che il panorama delle API web continua ad espandersi, `UrlFetchApp` rimane un ponte critico per i programmi Google Apps Script per interagire con il mondo oltre l'ecosistema di Google.
