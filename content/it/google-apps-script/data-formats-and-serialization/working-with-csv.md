---
title:                "Lavorare con CSV"
aliases:
- /it/google-apps-script/working-with-csv/
date:                  2024-02-01T22:05:56.553234-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con file CSV (Comma-Separated Values - Valori Separati da Virgola) in Google Apps Script comporta la lettura, modifica e scrittura di file di testo semplice dove ogni riga rappresenta un record di dati con valori separati da virgole. I programmatori fanno ciò per scambiare dati facilmente tra diverse applicazioni, database o linguaggi di programmazione data l'ampia adozione del CSV come formato di interscambio dati semplice e basato sul testo.

## Come fare:

### Leggere dati CSV

Per leggere dati CSV da un file memorizzato in Google Drive, è necessario prima ottenere il contenuto del file come stringa, poi analizzarlo. Google Apps Script rende semplice ottenere il contenuto del file con il servizio DriveApp.

```javascript
function readCSV() {
  var fileId = 'ID_DEL_TUO_FILE_QUI'; // Sostituire con l'ID del file effettivo
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Registra le celle di ogni riga
  }
}
```

### Scrivere dati CSV

Creare e scrivere un CSV comporta la costruzione di una stringa con valori separati da virgole e interruzioni di riga, poi salvarla o esportarla. Questo esempio dimostra come creare un nuovo file CSV in Google Drive.

```javascript
function writeCSV() {
  var folderId = 'ID_DELLA_TUA_CARTLLA_QUI'; // Sostituire con l'ID della cartella di Drive dove verrà creato il nuovo file
  var csvContent = "Nome,Età,Occupazione\nJohn Doe,29,Ingegnere\nJane Smith,34,Designer";
  var fileName = "esempio.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Esempio di Output

Quando si registrano le celle delle righe leggendo un CSV:

```plaintext
[John, 29, Ingegnere]
[Jane, 34, Designer]
```

Quando si scrive, viene creato un file denominato "esempio.csv" con il seguente contenuto:

```plaintext
Nome,Età,Occupazione
John Doe,29,Ingegnere
Jane Smith,34,Designer
```

## Approfondimento

Storicamente, i file CSV sono stati preferiti per la loro semplicità e leggibilità, rendendoli accessibili anche ai non programmatori e utili per compiti di ispezione dati rapidi. Tuttavia, Google Apps Script opera nell'ambito dell'ecosistema di Google, dove Google Fogli agisce come un'alternativa potente e user-friendly per la manipolazione di CSV. Fogli non solo fornisce una GUI per la modifica dei dati, ma supporta anche formule complesse, stili e molte altre funzionalità che i CSV grezzi non hanno.

Nonostante i vantaggi offerti da Google Fogli, la manipolazione diretta dei CSV in Google Apps Script rimane importante per compiti automatizzati, soprattutto quando si tratta di sistemi esterni che generano o richiedono dati in formato CSV. Ad esempio, l'integrazione con sistemi legacy, l'esportazione di dati per l'uso in altre applicazioni, o il pre-processing prima dell'inserimento dei dati in Google Fogli.

Inoltre, la capacità di Google Apps Script di lavorare con i file CSV può essere estesa con il servizio Utilities per esigenze di codifica avanzate, o interfacciate con API esterne per attività di conversione, analisi o validazione. Tuttavia, per lavorare con dataset di grandi dimensioni o che richiedono manipolazioni complesse, considerare di sfruttare le API di Google Fogli o esplorare BigQuery per capacità di elaborazione dati più robuste.

Sebbene la semplicità rimanga un motivo chiave della popolarità del CSV, queste alternative offrono un insieme di funzionalità più ricco per gestire i dati nell'ampio ecosistema di Google Cloud.
