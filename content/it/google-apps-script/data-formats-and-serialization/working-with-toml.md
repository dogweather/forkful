---
aliases:
- /it/google-apps-script/working-with-toml/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:16.165275-07:00
description: "TOML, che sta per Tom's Obvious, Minimal Language (Linguaggio Minimo\
  \ e Ovvio di Tom), \xE8 un formato di file di configurazione facile da leggere grazie\
  \ alla\u2026"
lastmod: 2024-02-18 23:08:55.503316
model: gpt-4-0125-preview
summary: "TOML, che sta per Tom's Obvious, Minimal Language (Linguaggio Minimo e Ovvio\
  \ di Tom), \xE8 un formato di file di configurazione facile da leggere grazie alla\u2026"
title: Lavorare con TOML
---

{{< edit_this_page >}}

## Cosa & Perché?

TOML, che sta per Tom's Obvious, Minimal Language (Linguaggio Minimo e Ovvio di Tom), è un formato di file di configurazione facile da leggere grazie alla sua chiara semantica. I programmatori spesso lo usano per i file di configurazione delle applicazioni perché è diretto e leggibile dall'uomo, rendendo la gestione delle impostazioni e delle configurazioni delle applicazioni senza problemi tra diversi ambienti.

## Come fare:

Poiché Google Apps Script è essenzialmente JavaScript con accesso alla suite di app di Google, lavorare direttamente con TOML all'interno di Google Apps Script richiede un po' di ingegnosità. Google Apps Script non supporta nativamente l'elaborazione di TOML, ma è possibile sfruttare le librerie JavaScript o scrivere un semplice parser per le esigenze di base.

Vediamo come analizzare una semplice stringa di configurazione TOML come esempio:

```javascript
// Stringa TOML
var tomlString = `
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
`;

// Funzione parser semplice da TOML a JSON
function parseTOML(tomlStr) {
  var result = {};
  var currentSection = result;
  tomlStr.split(/\r?\n/).forEach(line => {
    line = line.trim();
    if (line.startsWith('[')) { // Nuova sezione
      var sectionName = line.replace(/\[|\]/g, '');
      result[sectionName] = {};
      currentSection = result[sectionName];
    } else if (line) {
      var keyValue = line.split('=').map(part => part.trim());
      var key = keyValue[0];
      var value = eval(keyValue[1]); // Usa eval per semplicità; attenzione nel codice di produzione
      currentSection[key] = value;
    }
  });
  return result;
}

// Testa il parser
var configObject = parseTOML(tomlString);
console.log(configObject);
```

L'output di esempio da `console.log` assomiglierebbe a un oggetto JSON, rendendo più facile accedere alle proprietà di configurazione all'interno di Google Apps Script:

```json
{
  "database": {
    "server": "192.168.1.1",
    "ports": [8001, 8001, 8002],
    "connection_max": 5000,
    "enabled": true
  }
}
```

## Approfondimento

TOML è stato creato da Tom Preston-Werner, uno dei fondatori di GitHub, per essere più amichevole per l'uomo rispetto a JSON per i file di configurazione, pur mantenendo la capacità di essere analizzato in modo univoco. Si prefigge di essere il più semplice possibile, un obiettivo che si allinea bene con l'etica di molti progetti di sviluppo che mirano alla semplicità e leggibilità nei loro codici.

Nel contesto di Google Apps Script, usare TOML può introdurre un po' di sovraccarico, data la mancanza di supporto diretto e la necessità di analizzarlo manualmente o attraverso librerie di terze parti. Per progetti più piccoli o quelli non profondamente integrati nell'ecosistema di Google, alternative come JSON o anche semplici strutture di coppie chiave-valore nelle proprietà dello script potrebbero essere sufficienti e più semplici da implementare. Tuttavia, per applicazioni che danno priorità ai file di configurazione amichevoli per l'uomo e sono già impegnate con TOML, integrare l'analisi TOML attraverso script personalizzati aggiunge un utile strato di flessibilità e mantenibilità senza allontanarsi dai paradigmi di configurazione preferiti.
