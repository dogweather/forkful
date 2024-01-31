---
title:                "Arbeiten mit YAML"
date:                  2024-01-19
simple_title:         "Arbeiten mit YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist eine einfache Datenserialisierungssprache. Entwickler nutzen YAML oft wegen seiner Klarheit und Menschlesbarkeit, um Konfigurationen zu definieren und Daten zu speichern oder zu übertragen.

## How to:
Verarbeiten einer YAML-Konfigdatei in TypeScript:

```typescript
import * as yaml from 'js-yaml';
import * as fs from 'fs';

// YAML-Datei lesen.
const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));

// Beispiel: Zugriff auf einen Wert und Ausgabe
console.log(doc['meineEinstellung']);
```

YAML-Datei `config.yaml`:
```yaml
meineEinstellung: Wert123
```

Konsolenausgabe:
```
Wert123
```

## Deep Dive
YAML, "YAML Ain't Markup Language", wurde Anfang der 2000er als Alternative zu XML und JSON entworfen. Im Vergleich zu JSON ist YAML benutzerfreundlicher zu lesen und zu schreiben, unterstützt aber keine Tabellen/Arrays direkt. Es wird intern oft in JSON oder Binärformate umgewandelt. JavaScript-Bibliotheken wie `js-yaml` ermöglichen das Parsen und Schreiben von YAML in TypeScript Projekten.

## See Also
- YAML-Spezifikation: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
- `js-yaml` GitHub Repository: [https://github.com/nodeca/js-yaml](https://github.com/nodeca/js-yaml)
- TypeScript Dokumentation: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
