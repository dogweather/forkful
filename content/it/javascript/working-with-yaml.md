---
title:                "Lavorare con YAML"
date:                  2024-01-19
html_title:           "Bash: Lavorare con YAML"
simple_title:         "Lavorare con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un format di dati facilissimo da scrivere e leggere. Programmatori lo usano per configurazioni, dati serializzati, e applicazioni di deployment perché è chiaro e supporta strutture complesse con facilità.

## How to:
Per gestire YAML in JavaScript, usiamo la libreria `js-yaml`. Ecco come:

1. Installa con npm:
   ```
   npm install js-yaml
   ```

2. Leggi un file YAML e convertilo in un oggetto JavaScript:
   ```Javascript
   const fs = require('fs');
   const yaml = require('js-yaml');

   let doc;
   try {
     doc = yaml.load(fs.readFileSync('path/to/your/file.yml', 'utf8'));
     console.log(doc);
   } catch (e) {
     console.error(e);
   }
   ```

3. Converti un oggetto JavaScript in una stringa YAML:
   ```Javascript
   const fs = require('fs');
   const yaml = require('js-yaml');

   const obj = { nome: "Mario", professione: "Sviluppatore" };

   try {
     const ymlStr = yaml.dump(obj);
     fs.writeFileSync('path/to/output/file.yml', ymlStr, 'utf8');
     console.log('File YAML scritto con successo.');
   } catch (e) {
     console.error(e);
   }
   ```

## Deep Dive
YAML (YAML Ain't Markup Language) è emerso nei primi anni 2000 come alternativa leggibile al formato XML. Nonostante JSON sia più compatto, YAML è scelto per la sua leggibilità, particolarmente in settings di DevOps, come Docker o Kubernetes. Funziona bene con strutture di dati complesse, supporta i commenti e può gestire dati in diversi tipi. È importante notare, tuttavia, che YAML può essere suscettibile a problemi di sicurezza come la "YAML bombing" se non lo si usa con attenzione.

## See Also
- Documentazione ufficiale js-yaml: https://github.com/nodeca/js-yaml
- YAML spec: https://yaml.org/spec/
- Guida alla sicurezza YAML: https://blog.checkpoint.com/2021/08/05/ask-the-experts-what-is-yaml-and-how-can-it-be-secure/
