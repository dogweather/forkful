---
title:                "Lavorare con YAML"
aliases:
- it/typescript/working-with-yaml.md
date:                  2024-02-03T19:26:49.797191-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
YAML, un linguaggio di serializzazione dati progettato per essere amichevole per l'utente, è spesso utilizzato per file di configurazione, messaggistica tra processi e memorizzazione dati. I programmatori si affidano a YAML per la sua leggibilità e facilità d'uso, specialmente quando si tratta di dati strutturati complessi, rendendolo una scelta eccellente per applicazioni sviluppate in TypeScript.

## Come fare:
Lavorare con YAML in TypeScript tipicamente comporta l'analisi del contenuto YAML in oggetti JavaScript e, possibilmente, la conversione di oggetti JavaScript di nuovo in YAML. Questo richiede un parser; una scelta popolare è `js-yaml`, una libreria che può essere facilmente integrata nei progetti TypeScript.

### Installare js-yaml
Prima, aggiungi `js-yaml` al tuo progetto:

```bash
npm install js-yaml
```

### Convertire YAML in Oggetto JavaScript
Immagina di avere un file YAML `config.yaml` con il seguente contenuto:

```yaml
database:
  host: localhost
  port: 5432
  username: utente
  password: pass
```

Puoi leggere e analizzare questo file in un oggetto JavaScript come segue:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Carica e analizza il file YAML
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Output Esempio:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "utente",
    "password": "pass"
  }
}
```

### Convertire Oggetto JavaScript in YAML
Se hai bisogno di fare il contrario e convertire un oggetto JavaScript in una stringa YAML, puoi usare `js-yaml` così:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Esempio",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Output Esempio:**

```yaml
title: Esempio
is_published: true
author:
  name: Jane Doe
  age: 34
```

Questo snippet converte un oggetto JavaScript in una stringa YAML e la stampa. Nella pratica, potresti scrivere questa stringa di nuovo su un file o usarla in altre parti della tua applicazione.
