---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases:
- de/typescript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:43.908869-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis in TypeScript existiert, ist für Dateiverwaltungsaufgaben essentiell, wie zum Beispiel das Lesen aus oder das Schreiben von Daten in Dateien, um sicherzustellen, dass Operationen nur auf gültigen Verzeichnissen ausgeführt werden. Diese Operation ist entscheidend, um Fehler zu vermeiden, die durch den Versuch, auf nicht vorhandene Verzeichnisse zuzugreifen oder diese zu manipulieren, entstehen können.

## Wie geht das:

TypeScript, ausgeführt in einer Node.js-Umgebung, ermöglicht es Ihnen zu überprüfen, ob ein Verzeichnis existiert, indem Sie das `fs` Modul verwenden, welches die Funktion `existsSync()` oder die asynchrone Funktion `access()` in Verbindung mit `constants.F_OK` bereitstellt.

### Verwendung von `fs.existsSync()`:

```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('Verzeichnis existiert.');
} else {
  console.log('Verzeichnis existiert nicht.');
}
```

### Verwendung von `fs.access()` mit `fs.constants.F_OK`:

```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('Verzeichnis existiert nicht.');
    return;
  }
  console.log('Verzeichnis existiert.');
});
```

**Beispielausgabe** für beide Methoden, unter der Annahme, dass das Verzeichnis existiert:
```
Verzeichnis existiert.
```

Und wenn nicht:
```
Verzeichnis existiert nicht.
```

### Verwendung einer Drittanbieterbibliothek - `fs-extra`:

`fs-extra` ist eine beliebte Drittanbieterbibliothek, die das eingebaute `fs` Modul erweitert und bequemere Funktionen bereitstellt.

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`Verzeichnis existiert: ${exists}`);
});
```

**Beispielausgabe** wenn das Verzeichnis existiert:
```
Verzeichnis existiert: true
```

Und wenn nicht:
```
Verzeichnis existiert: false
```
