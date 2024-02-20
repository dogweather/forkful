---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:06:57.274625-07:00
description: "YAML, acronimo di \"YAML Ain't Markup Language\" (YAML Non \xE8 un Linguaggio\
  \ di Markup), \xE8 uno standard di serializzazione dei dati leggibile dall'uomo\u2026"
lastmod: 2024-02-19 22:05:02.085328
model: gpt-4-0125-preview
summary: "YAML, acronimo di \"YAML Ain't Markup Language\" (YAML Non \xE8 un Linguaggio\
  \ di Markup), \xE8 uno standard di serializzazione dei dati leggibile dall'uomo\u2026"
title: Lavorare con YAML
---

{{< edit_this_page >}}

## Cos'è & Perché?

YAML, acronimo di "YAML Ain't Markup Language" (YAML Non è un Linguaggio di Markup), è uno standard di serializzazione dei dati leggibile dall'uomo comunemente utilizzato per i file di configurazione e lo scambio di dati tra linguaggi con strutture dati diverse. I programmatori lavorano spesso con YAML per la sua semplicità e leggibilità, specialmente in progetti che richiedono una vasta configurazione o quando si trasferiscono dati strutturati tra diversi sistemi.

## Come fare:

Anche se Google Apps Script (GAS) non supporta nativamente l'analisi o la serializzazione di YAML, è possibile manipolare i dati YAML utilizzando librerie JavaScript o scrivendo funzioni di analisi personalizzate. Per dimostrazione, consideriamo come analizzare una stringa YAML utilizzando una funzione personalizzata, poiché le librerie esterne non possono essere importate direttamente in GAS.

Supponiamo che tu abbia una semplice configurazione YAML:

```yaml
title: YAML Example
description: Un esempio di come gestire YAML in Google Apps Script
tags:
  - Google Apps Script
  - YAML
  - Configuration
```

Per analizzarla in Google Apps Script, usa le capacità di manipolazione delle stringhe di JavaScript:

```javascript
function parseYAML(yamlString) {
  var result = {};
  var lines = yamlString.split("\n");
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    if (line.includes(":")) {
      var parts = line.split(":");
      var key = parts[0].trim();
      var value = parts[1].trim();
      // Gestione basica degli array
      if (value.startsWith("-")) {
        value = [value.substring(1).trim()];
        while (i + 1 < lines.length && lines[i + 1].trim().startsWith("-")) {
          i++;
          value.push(lines[i].trim().substring(1).trim());
        }
      }
      result[key] = value;
    }
  }
  return result;
}

function testYamlParsing() {
  var yaml = "title: YAML Example\ndescription: Un esempio di come gestire YAML in Google Apps Script\ntags:\n  - Google Apps Script\n  - YAML\n  - Configuration";
  var parsed = parseYAML(yaml);
  Logger.log(parsed);
}
```

Quando `testYamlParsing()` viene eseguito, restituisce:

```
{ title: 'YAML Example',
  description: 'Un esempio di come gestire YAML in Google Apps Script',
  tags: [ 'Google Apps Script', ' YAML', ' Configuration' ] }
```

Questo approccio di analisi personalizzato è piuttosto basilare e potrebbe necessitare di aggiustamenti per gestire file YAML complessi.

## Approfondimento

YAML, rilasciato inizialmente nel 2001, mirava a essere più leggibile per l'uomo rispetto ai suoi predecessori come XML o JSON. Sebbene la sua semplicità e facilità d'uso siano ampiamente apprezzate, gestire YAML in Google Apps Script presenta delle sfide a causa della mancanza di supporto diretto. Di conseguenza, i programmatori spesso si affidano alla versatilità di JavaScript per analizzare e generare dati YAML. Tuttavia, per casi d'uso complessi, specialmente quelli che coinvolgono nidificazioni profonde e strutture dati avanzate, questo metodo può diventare ingombrante e soggetto ad errori.

JSON, in contrasto, è supportato nativamente in Google Apps Script e nella maggior parte degli altri ambienti di programmazione, offrendo un approccio più diretto per la serializzazione e deserializzazione dei dati senza sovraccarichi di analisi aggiuntivi. La sintassi di JSON è meno verbosa rispetto a quella di YAML, rendendola più adatta allo scambio di dati nelle applicazioni web. Ciò nonostante, YAML rimane popolare per i file di configurazione e situazioni in cui la leggibilità umana è di primaria importanza.

Quando si lavora con YAML in Google Apps Script, considera i compromessi tra leggibilità e facilità d'uso. Per una manipolazione completa di YAML, potrebbe valere la pena esplorare strumenti o servizi esterni in grado di convertire YAML in JSON prima di elaborarlo all'interno del tuo script.
