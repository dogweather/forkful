---
title:                "Lavorare con yaml."
html_title:           "Javascript: Lavorare con yaml."
simple_title:         "Lavorare con yaml."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
YAML è un formato di dati usato principalmente nei linguaggi di programmazione per rappresentare informazioni strutturate in modo semplice e leggibile per l'uomo. I programmatori lo utilizzano soprattutto per la sua facilità di lettura e scrittura.

## Come fare: 
Per lavorare con YAML in Javascript, è necessario utilizzare una libreria esterna chiamata "js-yaml". Ecco un esempio di codice che mostra come convertire un oggetto Javascript in una stringa YAML:

```javascript
const jsYaml = require('js-yaml');

let oggetto = {
  nome: 'Mario',
  cognome: 'Rossi',
  età: 30,
  hobbies: ['calcio', 'musica', 'giardinaggio']
};

let stringaYaml = jsYaml.safeDump(oggetto);
console.log(stringaYaml);
```

L'output sarà una stringa di testo formattata in YAML:

```yaml
nome: Mario
cognome: Rossi
età: 30
hobbies:
- calcio
- musica
- giardinaggio
```

## Approfondimento:
YAML (acronimo di "YAML Ain't Markup Language") è stato creato nel 2001 da Clark Evans ed è stato pensato come un formato di dati che fosse più semplice e leggibile rispetto a XML. È costruito su una sintassi simile a quella dei linguaggi di programmazione, utilizzando l'indentazione per definire la struttura dei dati.

Un'alternativa a YAML è JSON, che ha una sintassi simile ma è più verboso e adatto per la comunicazione tra le applicazioni, mentre YAML è più indicato per la memorizzazione di dati all'interno di un programma.

Per utilizzare la libreria "js-yaml" è necessario installarla tramite npm o yarn:

`npm install js-yaml` o `yarn add js-yaml`

## Vedi anche:
- Documentazione ufficiale di js-yaml: https://github.com/nodeca/js-yaml
- Guida completa a YAML: https://yaml.org/