---
title:                "Lavorare con YAML"
date:                  2024-01-19
simple_title:         "Lavorare con YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML è un formato per dati leggibili da umani, usato per configurazioni o dati da serializzare. Programmatori lo usano per la sua leggibilità e semplicità, ideale per file di configurazione o integrazione con sistemi di deployment e orchestrazione.

## How to:
Per lavorare con YAML in TypeScript, occorre prima un parser, come `js-yaml`. Installalo via npm:

```bash
npm install js-yaml
```

Ecco come leggere YAML da file e trasformarlo in un oggetto JavaScript:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

const fileContents = fs.readFileSync('config.yaml', 'utf8');
const data = yaml.load(fileContents);
console.log(data);
```

Assumendo che `config.yaml` sia:

```yaml
version: 1
services:
  web:
    image: 'node:14'
    ports:
      - '80:80'
```

L'output sarà:

```typescript
{ version: 1, services: { web: { image: 'node:14', ports: ['80:80'] } } }
```

## Deep Dive
YAML, abbreviazione di "YAML Ain't Markup Language", è emerso nei primi anni 2000 come alternativa a XML. Alternativi a YAML includono JSON e TOML, ma YAML è spesso preferito per la sua leggibilità. Implementando il parsing di YAML in TypeScript, bisogna considerare le potenziali vulnerabilità come il YAML bombing, quindi è essenziale utilizzare librerie aggiornate e ben mantenute.

## See Also
- Documentazione ufficiale YAML: https://yaml.org
- Libreria `js-yaml` GitHub Repository: https://github.com/nodeca/js-yaml
- Guide di YAML per il suo utilizzo con Kubernetes: https://kubernetes.io/docs/home/
- Confronto tra YAML e JSON: https://stackoverflow.com/questions/1726802/what-is-the-difference-between-yaml-and-json
