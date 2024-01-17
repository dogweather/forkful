---
title:                "Programmazione con yaml"
html_title:           "TypeScript: Programmazione con yaml"
simple_title:         "Programmazione con yaml"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Lavorare con YAML significa utilizzare un formato di dati leggibile dall'uomo che viene utilizzato principalmente per la configurazione dei file. I programmatori spesso lavorano con YAML per la sua semplice sintassi e per la sua flessibilità.

## Come fare:
Si può facilmente lavorare con YAML in TypeScript utilizzando la libreria ```yamljs```. Ecco un esempio di codice che mostra come leggere un file YAML e accedere ai suoi dati:

```TypeScript
const yaml = require('yamljs');
const config = yaml.load('./config.yml');
console.log(config.name); // output: "John Doe"
```

## Approfondimento:
Questa libreria è stata creata da Jason Diamond nel 2007 ed è disponibile per molti linguaggi di programmazione diversi. Alcune alternative familiarità sono JSON (che è il formato parente di YAML) e XML (che è stato molto utilizzato in passato). La libreria ```yamljs``` è basata su una libreria di parsing in JavaScript chiamata ```yamlparser``` e supporta anche la scrittura di file YAML.

## Guarda anche:
- Documentazione ufficiale di YAML: https://yaml.org/
- Libreria di parsing JS: https://github.com/jeremyfa/yamlparser
- Alternativa a JSON: https://www.json.org/
- Alternativa a XML: https://www.w3.org/XML/