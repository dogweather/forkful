---
title:                "Lavorare con TOML"
aliases:
- it/javascript/working-with-toml.md
date:                  2024-01-26T04:23:27.663270-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/working-with-toml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
TOML, acronimo di Tom's Obvious, Minimal Language, definisce come strutturare i file di configurazione. I programmatori lavorano con TOML perché è facile da leggere, scrivere e mappa bene su una tabella hash, rendendolo una scelta preferenziale per le configurazioni.

## Come fare:
Per lavorare con TOML in JavaScript, avrai bisogno di un parser come `@iarna/toml`. Prima di tutto, installalo: `npm install @iarna/toml`. Poi, analizza una stringa TOML in un oggetto JavaScript o trasforma un oggetto JavaScript in formato TOML.

```javascript
const toml = require('@iarna/toml');

// Analizza stringa TOML in oggetto JS
const tomlStr = `
title = "Esempio TOML"

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
`;

const parsedData = toml.parse(tomlStr);
console.log(parsedData);

// Converte oggetto JS in stringa TOML
const jsObject = {
  title: "Esempio TOML",
  database: {
    server: "192.168.1.1",
    ports: [8001, 8001, 8002]
  }
};

const tomlString = toml.stringify(jsObject);
console.log(tomlString);
```

## Approfondimento
TOML è stato rilasciato per la prima volta nel 2013 da Tom Preston-Werner, co-fondatore di GitHub. È stato progettato per sostituire altri formati, come INI, essendo più standardizzato e facile da analizzare. JSON e YAML sono alternative, ma possono risultare troppo complessi o troppo flessibili. Il vantaggio di TOML risiede nelle configurazioni statiche dove si preferisce un formato semplice e chiaro. Il suo design permette una mappatura diretta su una tabella hash, con chiavi e valori che corrispondono ai nomi delle proprietà e ai loro valori. Per una più ampia adozione, potresti aver bisogno di integrare strumenti che possono convertire tra TOML e altri formati a causa del variabile supporto dell'ecosistema.

## Vedi Anche
- Il repository GitHub ufficiale di TOML: https://github.com/toml-lang/toml
- Confronto tra TOML, YAML e JSON: https://gist.github.com/oconnor663/9aeb4ed56394cb013a20
- pacchetto npm `@iarna/toml`: https://www.npmjs.com/package/@iarna/toml
