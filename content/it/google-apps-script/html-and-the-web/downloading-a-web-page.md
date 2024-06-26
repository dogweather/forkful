---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:32.298891-07:00
description: "Come fare: In Google Apps Script, il servizio `UrlFetchApp` \xE8 fondamentale\
  \ per scaricare contenuti web. Di seguito \xE8 presente una guida passo dopo passo\
  \ e\u2026"
lastmod: '2024-03-13T22:44:42.953586-06:00'
model: gpt-4-0125-preview
summary: "In Google Apps Script, il servizio `UrlFetchApp` \xE8 fondamentale per scaricare\
  \ contenuti web."
title: Scaricare una pagina web
weight: 42
---

## Come fare:
In Google Apps Script, il servizio `UrlFetchApp` è fondamentale per scaricare contenuti web. Di seguito è presente una guida passo dopo passo e un semplice esempio che dimostra come recuperare e registrare il contenuto HTML di una pagina web:

1. **Operazione di Fetch Base:**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- Questo codice recupera il contenuto HTML di example.com e lo registra. È una dimostrazione diretta di come ottenere la sorgente di una pagina web senza alcun parametro aggiuntivo.

2. **Gestione dei Reindirizzamenti e HTTPS:**

Per HTTPS o la gestione dei reindirizzamenti, il codice rimane sostanzialmente lo stesso, ma considerare l'implementazione di una gestione degli errori o di opzioni specifiche per i reindirizzamenti:

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // Segue automaticamente i reindirizzamenti
    'muteHttpExceptions': true // Silenzia le possibili eccezioni per gestirle con grazia
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **Limiti di Tasso e Quote:**

Prestare attenzione alle quote di Google Apps Script; un utilizzo intensivo può richiedere una gestione degli errori per i limiti di tasso.

## Approfondimento
Storicamente, il download e la manipolazione dei contenuti web sono iniziati con semplici richieste HTTP, evolvendosi significativamente con l'avvento dei linguaggi di scripting. Google Apps Script consente l'esecuzione diretta di tali compiti all'interno dell’ecosistema G Suite, sfruttando l'infrastruttura robusta di Google. Il servizio `UrlFetchApp` è un elemento fondamentale di questa funzionalità, incapsulando richieste HTTP/S complesse in un'interfaccia a livello di applicazione più semplice.

Nonostante la sua comodità, Google Apps Script potrebbe non essere sempre il miglior strumento per lo scraping web di grande portata o quando è richiesto un post-processing complesso dei dati recuperati a causa dei limiti di tempo di esecuzione e delle quote imposte da Google. In tali casi, framework dedicati allo scraping web o linguaggi progettati per operazioni I/O asincrone, come Node.js con librerie come Puppeteer o Cheerio, potrebbero offrire maggiore flessibilità e potenza.

Inoltre, sebbene Google Apps Script sia uno strumento eccellente per l'integrazione con i servizi Google (come Fogli, Documenti e Drive) e per l'esecuzione di operazioni leggere di recupero dati, è fondamentale tenere presente le limitazioni dell'ambiente di esecuzione. Per compiti intensivi, considerare l'uso di Google Cloud Functions o dei servizi avanzati di Apps Script con risorse di calcolo esterne per l'elaborazione.
