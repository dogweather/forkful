---
title:                "Lavorare con YAML"
aliases:
- /it/javascript/working-with-yaml.md
date:                  2024-02-03T19:25:35.761279-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?

YAML, acronimo di "YAML Ain't Markup Language", è un formato di serializzazione dei dati leggibile dall'uomo. Gli sviluppatori lo utilizzano spesso per file di configurazione e scambio di dati tra linguaggi grazie alla sua semplicità e leggibilità rispetto a JSON o XML.

## Come Fare:

In JavaScript, lavorare con YAML comporta tipicamente l'uso di una libreria di terze parti, poiché il linguaggio non include un parser incorporato per YAML. Una delle librerie più popolari a questo scopo è `js-yaml`. Puoi usare `js-yaml` per analizzare YAML in oggetti JavaScript e viceversa.

Prima di tutto, devi installare `js-yaml`:

```bash
npm install js-yaml
```

Quindi, puoi usarlo nei tuoi progetti. Ecco come puoi caricare un file YAML e analizzarlo in un oggetto JavaScript:

```javascript
// Richiedi il modulo js-yaml
const yaml = require('js-yaml');
const fs   = require('fs');

// Carica YAML da un file
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch(e) {
  console.error(e);
}
```

Se il tuo file `config.yaml` è così:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

L'output sarà:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

Per fare il contrario, convertendo un oggetto JavaScript in una stringa YAML:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Questo codice produrrà:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Usando `js-yaml`, puoi facilmente integrare l'analisi e la serializzazione YAML nei tuoi progetti JavaScript, migliorando l'intercambiabilità dei dati e la gestione della configurazione.
