---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:31.929150-07:00
description: "Lavorare con XML in Google Apps Script permette ai programmatori di\
  \ analizzare, manipolare e generare dati XML, essenziali per i servizi web e le\u2026"
lastmod: 2024-02-19 22:05:02.090210
model: gpt-4-0125-preview
summary: "Lavorare con XML in Google Apps Script permette ai programmatori di analizzare,\
  \ manipolare e generare dati XML, essenziali per i servizi web e le\u2026"
title: Lavorare con XML
---

{{< edit_this_page >}}

## Cos'è e Perché?

Lavorare con XML in Google Apps Script permette ai programmatori di analizzare, manipolare e generare dati XML, essenziali per i servizi web e le configurazioni. I programmatori adottano questo approccio per integrarsi con sistemi legacy, eseguire web scraping o comunicare con numerose API che si affidano ancora a XML anziché a JSON per lo scambio di dati.

## Come fare:

Google Apps Script fornisce l'`XmlService` per lavorare con i dati XML. Qui di seguito dimostriamo come analizzare una stringa XML, modificarne il contenuto e generare una nuova stringa XML.

Analisi di una stringa XML:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Ciao</child><child name="second">Mondo</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Registra: Ciao
}
```

Per modificare l'XML, potresti voler aggiungere un nuovo elemento figlio:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Ciao</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('Mondo');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Registra la nuova stringa XML con l'elemento figlio aggiunto
}
```

Generare una stringa XML da zero:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Ciao Mondo');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Produce: <root><child>Ciao Mondo</child></root>
}
```

## Approfondimento

Storicamente, XML (Extensible Markup Language) era lo standard de facto per lo scambio di dati prima che JSON emergesse come alternativa leggera. La sintassi prolissa di XML e il suo modello di parsing rigoroso hanno fornito un formato di dati robusto, seppur ingombrante. In Google Apps Script, l'API `XmlService` racchiude la creazione, l'analisi e la manipolazione dei dati XML, riconoscendo la sua importanza continuata in vari sistemi legacy ed enterprise, nei servizi web SOAP e nei file di configurazione per le applicazioni.

Nonostante la prevalenza di JSON nello sviluppo web moderno per la sua semplicità e facilità d'uso con JavaScript, XML rimane rilevante in aree dove la validazione dei documenti e le gerarchie strutturate sono cruciali. Tuttavia, per nuovi progetti, specialmente quelli orientati verso le API web, JSON è spesso la scelta più pratica a causa della sua natura leggera e dell'integrazione senza soluzione di continuità con JavaScript.

Comprendere XML e il suo trattamento in Google Apps Script è fondamentale per gli sviluppatori che lavorano in ambienti dove è necessaria l'integrazione con sistemi più vecchi o specifiche API aziendali. Tuttavia, all'inizio di nuovi progetti o quando la flessibilità è chiave, valutare la necessità di XML rispetto ad alternative come JSON è consigliabile.
