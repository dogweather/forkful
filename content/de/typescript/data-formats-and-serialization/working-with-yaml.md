---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:56.764311-07:00
description: "YAML, eine zur Datenserialisierung entworfene Sprache, die benutzerfreundlich\
  \ sein soll, wird oft f\xFCr Konfigurationsdateien, Interprozesskommunikation und\u2026"
lastmod: '2024-02-25T18:49:50.719619-07:00'
model: gpt-4-0125-preview
summary: "YAML, eine zur Datenserialisierung entworfene Sprache, die benutzerfreundlich\
  \ sein soll, wird oft f\xFCr Konfigurationsdateien, Interprozesskommunikation und\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?
YAML, eine zur Datenserialisierung entworfene Sprache, die benutzerfreundlich sein soll, wird oft für Konfigurationsdateien, Interprozesskommunikation und Datenspeicherung verwendet. Programmierer bevorzugen YAML wegen seiner Lesbarkeit und einfachen Handhabung, besonders beim Umgang mit komplex strukturierten Daten, was es zu einer ausgezeichneten Wahl für in TypeScript entwickelte Anwendungen macht.

## Wie geht das:
Die Arbeit mit YAML in TypeScript beinhaltet typischerweise das Parsen von YAML-Inhalten in JavaScript-Objekte und möglicherweise das Umwandeln von JavaScript-Objekten zurück in YAML. Dies erfordert einen Parser; eine beliebte Wahl ist `js-yaml`, eine Bibliothek, die leicht in TypeScript-Projekte integriert werden kann.

### js-yaml installieren
Fügen Sie zunächst `js-yaml` zu Ihrem Projekt hinzu:

```bash
npm install js-yaml
```

### YAML in JavaScript-Objekt parsen
Stellen Sie sich vor, Sie haben eine YAML-Datei `config.yaml` mit folgendem Inhalt:

```yaml
database:
  host: localhost
  port: 5432
  username: user
  password: pass
```

Sie können diese Datei wie folgt in ein JavaScript-Objekt lesen und parsen:

```typescript
import * as fs from 'fs';
import * as yaml from 'js-yaml';

// Laden und parsen Sie die YAML-Datei
const fileContents = fs.readFileSync('./config.yaml', 'utf8');
const data = yaml.load(fileContents) as Record<string, any>;

console.log(data);
```

**Beispielausgabe:**

```json
{
  "database": {
    "host": "localhost",
    "port": 5432,
    "username": "user",
    "password": "pass"
  }
}
```

### JavaScript-Objekt in YAML umwandeln
Wenn Sie den umgekehrten Weg gehen müssen und ein JavaScript-Objekt in einen YAML-String umwandeln müssen, können Sie `js-yaml` wie folgt verwenden:

```typescript
import * as yaml from 'js-yaml';

const obj = {
  title: "Beispiel",
  is_published: true,
  author: {
    name: "Jane Doe",
    age: 34
  }
};

const yamlStr = yaml.dump(obj);
console.log(yamlStr);
```

**Beispielausgabe:**

```yaml
title: Beispiel
is_published: true
author:
  name: Jane Doe
  age: 34
```

Dieser Schnipsel konvertiert ein JavaScript-Objekt in einen YAML-String und gibt ihn aus. In der Praxis könnten Sie dies zurück in eine Datei schreiben oder in anderen Teilen Ihrer Anwendung verwenden.
