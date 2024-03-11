---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:41.006016-07:00
description: "YAML, die Abk\xFCrzung f\xFCr YAML Ain't Markup Language, ist ein menschenlesbares\
  \ Daten-Serialisierungsformat. Programmierer verwenden es oft f\xFCr\u2026"
lastmod: '2024-03-11T00:14:28.191559-06:00'
model: gpt-4-0125-preview
summary: "YAML, die Abk\xFCrzung f\xFCr YAML Ain't Markup Language, ist ein menschenlesbares\
  \ Daten-Serialisierungsformat. Programmierer verwenden es oft f\xFCr\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?

YAML, die Abkürzung für YAML Ain't Markup Language, ist ein menschenlesbares Daten-Serialisierungsformat. Programmierer verwenden es oft für Konfigurationsdateien und den Datenaustausch zwischen Sprachen aufgrund seiner Einfachheit und Lesbarkeit im Vergleich zu JSON oder XML.

## Wie zu:

Bei der Arbeit mit YAML in JavaScript ist typischerweise die Verwendung einer Drittanbieterbibliothek erforderlich, da die Sprache keinen eingebauten Parser für YAML enthält. Eine der beliebtesten Bibliotheken für diesen Zweck ist `js-yaml`. Mit `js-yaml` können Sie YAML in JavaScript-Objekte parsen und umgekehrt.

Zuerst müssen Sie `js-yaml` installieren:

```bash
npm install js-yaml
```

Dann können Sie es in Ihren Projekten verwenden. So können Sie eine YAML-Datei laden und in ein JavaScript-Objekt parsen:

```javascript
// Das js-yaml-Modul einbinden
const yaml = require('js-yaml');
const fs   = require('fs');

// YAML aus einer Datei laden
try {
  const doc = yaml.load(fs.readFileSync('./config.yaml', 'utf8'));
  console.log(doc);
} catch (e) {
  console.error(e);
}
```

Wenn Ihre `config.yaml`-Datei so aussieht:

```yaml
version: 1
services:
  web:
    image: "myapp/web:latest"
    ports:
      - "5000:5000"
```

Wird die Ausgabe sein:

```javascript
{ version: 1,
  services: 
   { web: 
      { image: 'myapp/web:latest',
        ports: [ '5000:5000' ] } } }
```

Um das Gegenteil zu tun, also ein JavaScript-Objekt in einen YAML-String zu konvertieren:

```javascript
const yaml = require('js-yaml');
const obj = {
  version: 1,
  services: {
    web: {
      image: "myapp/web:latest",
      ports: ["5000:5000"]
    }
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

Dieser Code wird produzieren:

```yaml
version: 1
services:
  web:
    image: myapp/web:latest
    ports:
      - '5000:5000'
```

Mit `js-yaml` können Sie die YAML-Verarbeitung und -Serialisierung problemlos in Ihre JavaScript-Projekte integrieren, um den Datenaustausch und das Konfigurationsmanagement zu verbessern.
