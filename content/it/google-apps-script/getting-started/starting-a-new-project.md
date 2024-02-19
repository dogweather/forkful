---
aliases:
- /it/google-apps-script/starting-a-new-project/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:46.370222-07:00
description: "Avviare un nuovo progetto in Google Apps Script (GAS) comporta l'inizializzazione\
  \ di un file script all'interno dell'ecosistema Google (Google Drive,\u2026"
lastmod: 2024-02-18 23:08:55.478410
model: gpt-4-0125-preview
summary: "Avviare un nuovo progetto in Google Apps Script (GAS) comporta l'inizializzazione\
  \ di un file script all'interno dell'ecosistema Google (Google Drive,\u2026"
title: Iniziando un nuovo progetto
---

{{< edit_this_page >}}

## Cosa e Perché?

Avviare un nuovo progetto in Google Apps Script (GAS) comporta l'inizializzazione di un file script all'interno dell'ecosistema Google (Google Drive, Docs, Sheets, ecc.) per automatizzare compiti o ampliare le funzionalità delle Google Apps. I programmatori intraprendono spesso questo percorso per semplificare i flussi di lavoro, manipolare programmabilmente i servizi di Google o creare componenti aggiuntivi personalizzati, risparmiando tempo e sfruttando la potenza dell'infrastruttura di Google.

## Come Fare:

Per iniziare un nuovo progetto in Google Apps Script, hai un paio di punti di accesso, ma concentriamoci sul metodo più diretto: creare uno script da Google Drive.

1. **Creare un Progetto in Google Drive**
   - Naviga verso Google Drive (drive.google.com).
   - Clicca su "+ Nuovo" > "Altro" > "Google Apps Script".
   - Si apre un nuovo progetto script nell'editor. Per impostazione predefinita, contiene un file `Code.gs` con un esempio di funzione `myFunction`.

2. **Configurare il Tuo Progetto**
   - Rinomina il tuo progetto per chiarezza. Clicca su "Progetto senza titolo" in alto a sinistra e dagli un nome significativo.
   - Scrivi una semplice funzione nel file `Code.gs` per prenderci la mano:

```javascript
function helloWorld() {
  Logger.log('Ciao, mondo!');
}
```

   - Esegui `helloWorld` selezionando la funzione nel menu a tendina accanto al pulsante di avvio (▶) e cliccandolo. Questo eseguirà la funzione.

3. **Visualizzare i Log**
   - Per visualizzare l'output di `Logger.log`, vai su "Visualizza" > "Log", o premi `Ctrl + Invio`. Dovresti vedere "Ciao, mondo!" nei log.

Congratulazioni, hai appena avviato con successo un nuovo progetto in Google Apps Script ed eseguito una semplice funzione!

## Approfondimento

L'introduzione di Google Apps Script nel 2009 ha fornito una piattaforma potente ma accessibile sia per sviluppatori che per non sviluppatori per automatizzare, estendere e costruire sull'ampia gamma di servizi Google. A differenza degli ambienti di programmazione tradizionali, GAS offre una combinazione unica di semplicità e integrazione, direttamente all'interno dell'ecosistema Google, senza la necessità di server esterni o configurazioni. Questo modello di esecuzione serverless semplifica enormemente il dispiegamento e la gestione dei progetti.

Storicamente, GAS era in qualche modo limitato dal suo ambiente di esecuzione e dalla versione del linguaggio, spesso in ritardo rispetto agli standard JavaScript attuali. Tuttavia, gli aggiornamenti recenti hanno portato la sintassi JavaScript moderna (ECMAScript 2015+) in GAS, rendendolo più appetibile per gli sviluppatori abituati alle pratiche di sviluppo contemporanee.

Sebbene GAS sia posizionato in modo unico per interagire con i Servizi Google, esistono approcci alternativi per esigenze più intensive o specifiche. Ad esempio, Google Cloud Functions e Google Cloud Platform (GCP) offrono soluzioni più robuste e scalabili per gestire flussi di lavoro complessi, elaborare grandi set di dati e integrarsi con API esterne. Queste piattaforme consentono la programmazione in varie lingue (ad es., Python, Go, Node.js) e offrono maggiori risorse computazionali.

Tuttavia, per compiti strettamente legati alle Google Apps, all'automazione e allo sviluppo rapido all'interno di questo ecosistema, Google Apps Script rimane uno strumento senza pari in termini di facilità d'uso e profondità di integrazione. La sua accessibilità direttamente da Google Drive e la connessione senza soluzione di continuità ai servizi Google lo rendono una scelta pratica per una vasta gamma di progetti, in particolare per coloro che cercano di estendere la funzionalità di Sheets, Docs, Forms e altre applicazioni Google.
