---
title:                "Ricerca e sostituzione del testo"
date:                  2024-02-01T22:00:50.249827-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ricerca e sostituzione del testo"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

La ricerca e sostituzione di testo in Google Apps Script consiste nell'identificare in modo programmato specifiche stringhe in un documento, foglio di calcolo o qualsiasi altro tipo di contenuto di Google Apps, e nel sostituirle con altri valori testuali. I programmatori utilizzano questa funzionalità per automatizzare la modifica di grandi volumi di contenuto, correggere errori comuni, standardizzare la terminologia tra diversi documenti o inserire dati dinamici in modelli.

## Come fare:

Google Apps Script offre un modo semplice per cercare e sostituire testo, specialmente all'interno di Google Docs e Fogli. Di seguito sono riportati esempi per entrambi.

### Google Docs:

Per cercare e sostituire testo in un Documento Google, si interagirà principalmente con la classe `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var corpo = doc.getBody();
  
  // Per cercare e sostituire una frase specifica
  corpo.replaceText('testoDaCercare', 'testoDiSostituzione');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Utilizzo
searchReplaceInDoc();
```

Questo frammento di codice cerca tutte le occorrenze di `'testoDaCercare'` nel Documento Google attivo e le sostituisce con `'testoDiSostituzione'`.

### Google Sheets:

Analogamente, in Google Sheets, si può utilizzare `SpreadsheetApp` per eseguire operazioni di ricerca e sostituzione:

```javascript
function searchReplaceInSheet() {
  var foglio = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Cerca e sostituisce nel foglio attivo corrente
  // replaceText(testoDaCercare, testoDiSostituzione)
  foglio.createTextFinder('testoDaCercare').replaceAllWith('testoDiSostituzione');
}

// Utilizzo
searchReplaceInSheet();
```

In questo esempio, `createTextFinder('testoDaCercare')` cerca nel foglio attivo 'testoDaCercare', e `replaceAllWith('testoDiSostituzione')` sostituisce tutte le occorrenze con 'testoDiSostituzione'.

## Approfondimento

La funzionalità di ricerca e sostituzione in Google Apps Script è fortemente influenzata dalla sua natura basata sul web, consentendo agli script di manipolare testo in vari Google Apps senza soluzione di continuità. Storicamente, questa capacità deriva dal contesto più ampio della elaborazione e manipolazione di testo nella programmazione, dove le espressioni regolari e le funzioni di stringa in linguaggi come Perl e Python hanno stabilito un alto standard per flessibilità e potenza.

Mentre la funzionalità di ricerca e sostituzione di Google Apps Script è potente per sostituzioni semplici, manca delle complete capacità di espressioni regolari trovate in alcuni altri linguaggi. Ad esempio, mentre è possibile utilizzare espressioni regolari di base in `createTextFinder` in Google Sheets, le opzioni per il pattern matching complesso e la manipolazione sono limitate rispetto a Perl o Python.

Per esigenze di elaborazione del testo più avanzate, i programmatori potrebbero ricorrere all'esportazione del contenuto di Google Docs o Fogli in un formato che può essere elaborato esternamente con linguaggi più potenti o impiegando Google Apps Script per chiamare API esterne o servizi che offrono capacità di manipolazione del testo più sofisticate.

Nonostante queste limitazioni, per la maggior parte dei compiti tipici di ricerca e sostituzione all'interno dell'ecosistema di Google Apps, Google Apps Script offre una soluzione semplice, efficiente e altamente integrabile, adatta alle esigenze di automazione e scripting all'interno della suite di strumenti di produttività di Google.
