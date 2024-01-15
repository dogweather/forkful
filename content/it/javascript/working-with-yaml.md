---
title:                "Lavorare con yaml"
html_title:           "Javascript: Lavorare con yaml"
simple_title:         "Lavorare con yaml"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Javascript, molto probabilmente conosci già il formato YAML. Ma forse non hai mai pensato a utilizzarlo nei tuoi progetti. Ecco perché dovresti dare una possibilità a YAML: è un formato leggibile dall'uomo e molto utile per organizzare e strutturare dati complessi.

## Come

Per utilizzare YAML in Javascript, abbiamo bisogno di un modulo chiamato "js-yaml". Per installarlo, apri il tuo terminale e digita il seguente comando:

```Javascript
npm install js-yaml
```

Una volta installato il modulo, possiamo iniziare a utilizzarlo nel nostro codice. Di seguito un esempio di come leggere e scrivere un file YAML utilizzando il modulo "js-yaml":

```Javascript
const yaml = require('js-yaml');
const fs = require('fs');

// Leggiamo il file YAML
const doc = yaml.safeLoad(fs.readFileSync('miofile.yml', 'utf8'));

// Modifichiamo i dati letti
doc.name = 'Nuovo nome';
doc.age = 30;

// Scriviamo il file YAML modificato
fs.writeFileSync('miofile.yml', yaml.safeDump(doc));
```

L'output ottenuto sarà un file YAML con i dati modificati, pronto per essere utilizzato nel tuo progetto!

## Deep Dive

Per coloro che vogliono approfondire la conoscenza di YAML, ci sono molte altre funzionalità e opzioni disponibili. Ad esempio, il modulo "js-yaml" ci permette di impostare diversi opzioni durante la lettura e la scrittura dei file YAML, come ad esempio una maggiore sicurezza dei dati o una formattazione personalizzata.

Inoltre, è importante notare che YAML supporta anche l'utilizzo di commenti nei file, rendendolo ancora più flessibile e facile da leggere per gli esseri umani.

## Vedi anche

- [Documentazione ufficiale di JS-YAML](https://github.com/nodeca/js-yaml)
- [Tutorial sul formato YAML](https://www.tutorialspoint.com/yaml/index.htm)
- [Un'introduzione a YAML per programmatori Javascript](https://lucasoft.blog/cosa-e-yaml-per-che-cosa-si-usa)