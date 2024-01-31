---
title:                "Lavorare con TOML"
date:                  2024-01-26T04:27:00.624311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-toml.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
TOML, acronimo di Tom's Obvious, Minimal Language, è un formato di serializzazione dei dati simile a JSON o YAML. I programmatori lo utilizzano per la sua leggibilità umana e per il mapping diretto ai tipi di dati, rendendolo una scelta prediletta per i file di configurazione e lo scambio di dati.

## Come fare:
Per prima cosa, avrai bisogno di un parser TOML. `@iarna/toml` è una scelta popolare. Installalo con npm: `npm install @iarna/toml --save`. Ecco come leggere un file TOML e analizzarlo in un oggetto JavaScript:

```typescript
import * as fs from 'fs';
import toml from '@iarna/toml';

const tomlContent = fs.readFileSync('config.toml', 'utf-8');
const parsedData = toml.parse(tomlContent);

console.log(parsedData);
```
Se `config.toml` contiene:
```
[server]
port = 8080
```
L'output sarà:
```
{ server: { port: 8080 } }
```
E, scrivere su un file TOML è altrettanto diretto:
```typescript
import * as fs from 'fs';
import { stringify } from '@iarna/toml';

const obj = { server: { port: 8080 } };
const tomlString = stringify(obj);
fs.writeFileSync('config.toml', tomlString);
``` 
Eseguendo questo codice si scrive l'oggetto in `config.toml` in formato TOML.

## Approfondimento
TOML è stato creato da Tom Preston-Werner, co-fondatore di GitHub, intorno al 2013 come risposta alle limitazioni che percepiva in altri formati come INI o YAML. È progettato per essere univoco e facile da analizzare in strutture dati, quindi, un preferito per i file di configurazione. Alternative come JSON mancano di commenti, mentre YAML è più complesso. TOML brilla nella sua semplicità e nella sua capacità di rappresentare chiaramente gerarchie di dati complesse.

Sotto il cofano, quando analizzi TOML in TypeScript, stai convertendo dati testuali in un formato strutturato che il linguaggio può manipolare. Ciò implica il lexing (trasformare il testo grezzo in token) e il parsing (costruire una struttura dati interna); `@iarna/toml` gestisce entrambi senza problemi. Il supporto emoji è un tocco divertente, che mostra l'approccio centrato sull'utente di TOML.

## Vedi anche
- Specifica ufficiale TOML: https://toml.io/en/
- pacchetto `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
- Confronti tra TOML, YAML e JSON: https://blog.bitsrc.io/choosing-the-right-configuration-file-format-toml-vs-yaml-vs-json-71b5be8968ea
