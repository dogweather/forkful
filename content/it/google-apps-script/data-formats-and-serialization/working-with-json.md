---
title:                "Lavorare con JSON"
aliases:
- /it/google-apps-script/working-with-json/
date:                  2024-02-01T22:05:43.845697-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

JSON, o JavaScript Object Notation, è un formato leggero per l'archiviazione e il trasporto dei dati, ideale per la comunicazione server-client e i file di configurazione. Gli sviluppatori lo sfruttano in Google Apps Script per uno scambio di dati senza soluzione di continuità tra i servizi Google (come Fogli, Documenti, Drive) e fonti esterne, grazie alla sua struttura leggibile dall'uomo e alla facile integrazione negli ambienti basati su JavaScript.

## Come fare:

In Google Apps Script, manipolare JSON è un processo diretto, principalmente a causa del supporto nativo che JavaScript offre per il parsing e la stringificazione di JSON. Ecco alcune operazioni comuni:

**1. Parsing di JSON**: Supponiamo di recuperare una stringa JSON da un servizio web; analizzarla per trasformarla in un oggetto JavaScript è essenziale per la manipolazione dei dati.

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // Output: Sample Project
```

**2. Stringificare Oggetti JavaScript**: Al contrario, convertire un oggetto JavaScript in una stringa JSON è utile quando dobbiamo inviare dati da Apps Script a un servizio esterno.

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // Output: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. Lavorare con Dati Complessi**:
Per strutture dati più complesse, come array di oggetti, il processo rimane lo stesso, dimostrando la flessibilità di JSON per la rappresentazione dei dati.

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // Output: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## Approfondimento

L'onnipresenza di JSON nelle applicazioni web moderne non può essere sottovalutata, radicata nella sua semplicità e nell'integrazione senza soluzione di continuità con JavaScript, il linguaggio del web. Il suo design, ispirato ai letterali degli oggetti JavaScript, sebbene più rigoroso, facilita la sua rapida adozione. Nei primi anni 2000, JSON ha guadagnato popolarità come alternativa a XML per le applicazioni web basate su AJAX, offrendo un formato di scambio dati più leggero e meno verboso. Data l'integrazione profonda di Google Apps Script con varie API di Google e servizi esterni, JSON funge da formato pivotale per strutturare, trasportare e manipolare dati attraverso queste piattaforme.

Sebbene JSON regni sovrano per le applicazioni web, esistono formati di dati alternativi come YAML per i file di configurazione o Protobuf per una serializzazione binaria più efficiente in ambienti ad alte prestazioni. Tuttavia, l'equilibrio di JSON tra leggibilità, facilità d'uso e ampio supporto attraverso linguaggi di programmazione e strumenti ne consolida la posizione come scelta predefinita per molti sviluppatori che si avventurano in Google Apps Script e oltre.
