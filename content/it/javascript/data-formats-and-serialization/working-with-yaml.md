---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:35.761279-07:00
description: "Come Fare: In JavaScript, lavorare con YAML comporta tipicamente l'uso\
  \ di una libreria di terze parti, poich\xE9 il linguaggio non include un parser\u2026"
lastmod: '2024-03-13T22:44:43.833944-06:00'
model: gpt-4-0125-preview
summary: "In JavaScript, lavorare con YAML comporta tipicamente l'uso di una libreria\
  \ di terze parti, poich\xE9 il linguaggio non include un parser incorporato per\
  \ YAML."
title: Lavorare con YAML
weight: 41
---

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
