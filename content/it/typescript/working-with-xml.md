---
title:                "Lavorare con XML"
date:                  2024-01-26T04:36:09.367418-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-xml.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Lavorare con XML significa analizzare, manipolare e scrivere dati XML utilizzando la programmazione. I programmatori gestiscono XML per lo scambio di dati tra diversi sistemi, per file di configurazione o quando si lavora con standard come SOAP che si affidano a XML.

## Come fare:
```TypeScript
import { parseString } from 'xml2js';

// Esempio di XML
const xml = `<note>
                <to>Utente</to>
                <from>Autore</from>
                <heading>Promemoria</heading>
                <body>Non dimenticare l'incontro!</body>
             </note>`;

// Analizza XML in JSON
parseString(xml, (err, result) => {
    if(err) throw err;
    console.log(result);
});

// Assumendo che l'analisi sia stata eseguita con successo, l'output potrebbe essere simile a:
// { note:
//    { to: ['Utente'],
//      from: ['Autore'],
//      heading: ['Promemoria'],
//      body: ['Non dimenticare l'incontro!'] } 
}
```

## Approfondimento
XML, o Extensible Markup Language, esiste dalla fine degli anni '90. La sua natura auto-descrittiva e il formato facilmente leggibile dall'uomo lo hanno reso popolare fin dall'inizio per varie applicazioni come i feed RSS, la gestione della configurazione e persino i formati di documenti d'ufficio come Microsoft Office Open XML. Tuttavia, risulta verboso rispetto a JSON, e la tendenza si sta invertendo. JSON ha guadagnato i riflettori per le API basate sul web grazie alla sua leggerezza e compatibilità nativa con JavaScript.

Nonostante ciò, XML non è morto. Viene utilizzato in sistemi aziendali su larga scala e per standard documentali che non sono passati a JSON. Strumenti come `xml2js` per TypeScript o `lxml` in Python dimostrano che c'è ancora bisogno di manipolare XML nella programmazione.

TypeScript non ha un supporto integrato per XML come lo ha per JSON. Invece, si lavora con librerie. `xml2js` è un esempio. Trasforma l'XML in JSON, rendendo i dati più facili da gestire per i guru di JavaScript.

## Vedi Anche
- [MDN Web Docs su XML](https://developer.mozilla.org/en-US/docs/Web/XML/XML_introduction)
- [Pacchetto npm xml2js](https://www.npmjs.com/package/xml2js)
- [Tutorial XML di W3Schools](https://www.w3schools.com/xml/)
