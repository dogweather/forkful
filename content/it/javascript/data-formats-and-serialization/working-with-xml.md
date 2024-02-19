---
aliases:
- /it/javascript/working-with-xml/
date: 2024-01-26 04:32:43.302178-07:00
description: "Lavorare con XML significa analizzare, manipolare e produrre contenuti\
  \ XML mediante codice. I programmatori lo fanno perch\xE9 XML \xE8 ampiamente utilizzato\u2026"
lastmod: 2024-02-18 23:08:56.276589
model: gpt-4-0125-preview
summary: "Lavorare con XML significa analizzare, manipolare e produrre contenuti XML\
  \ mediante codice. I programmatori lo fanno perch\xE9 XML \xE8 ampiamente utilizzato\u2026"
title: Lavorare con XML
---

{{< edit_this_page >}}

## Cosa e Perché?
Lavorare con XML significa analizzare, manipolare e produrre contenuti XML mediante codice. I programmatori lo fanno perché XML è ampiamente utilizzato per file di configurazione, scambio di dati e servizi web grazie alla sua natura leggibile dall'uomo e analizzabile dalla macchina.

## Come fare:

Ecco come analizzare XML:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>Utente</to>
                    <from>Autore</from>
                    <heading>Promemoria</heading>
                    <body>Non dimenticarmi questo fine settimana!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// Output: Utente
```

E per produrre XML:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'Utente';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// Output: <note><to>Utente</to></note>
```

## Approfondimento

XML è l'acronimo di eXtensible Markup Language, un formato di dati presente fin dalla fine degli anni '90. Definisce un insieme di regole per la codifica di documenti leggibili sia dall'uomo che dalla macchina. Storicamente, XML ha guadagnato terreno per la sua flessibilità e struttura gerarchica, rendendolo una scelta prevalente per servizi web, come SOAP, e numerosi file di configurazione.

Alternative a XML includono JSON (JavaScript Object Notation), che è diventato popolare per la sua facilità d'uso con JavaScript e per essere più leggero. YAML è un'altra alternativa, apprezzata per essere allo stesso tempo amichevole per gli umani e scelta comune per la configurazione.

XML è implementato in JavaScript tramite le interfacce DOMParser e XMLSerializer. Il DOM XML (Document Object Model) permette di navigare e modificare documenti XML proprio come si farebbe con HTML. Nonostante l'ascesa di JSON, comprendere XML è fondamentale, poiché numerosi sistemi legacy e specifici settori industriali si affidano ancora ad esso per lo scambio di dati.

## Vedi Anche

- MDN Web Docs (Analisi XML): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (Tutorial XML DOM): https://www.w3schools.com/xml/dom_intro.asp
- "Cos'è XML?": https://www.w3.org/XML/
