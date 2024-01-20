---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:58:57.857349-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Prüfen, ob ein Verzeichnis existiert, bedeutet, sicherzustellen, dass ein bestimmter Ordnerpfad im Dateisystem vorhanden ist. Programmierer machen das, um Fehler zu vermeiden, die auftreten können, wenn sie versuchen, auf ein nicht existierendes Verzeichnis zuzugreifen oder darin zu schreiben.

## So geht's:
Um in TypeScript zu überprüfen, ob ein Verzeichnis existiert, nutzen wir das `fs` Modul von Node.js. Hier ein kurzes Beispiel:

```typescript
import { existsSync } from 'fs';

const directoryPath = './meinVerzeichnis';

if (existsSync(directoryPath)) {
    console.log('Das Verzeichnis existiert.');
} else {
    console.log('Das Verzeichnis existiert nicht.');
}
```

Ausgabe, falls das Verzeichnis existiert:
```
Das Verzeichnis existiert.
```

Ausgabe, falls das Verzeichnis nicht existiert:
```
Das Verzeichnis existiert nicht.
```

## Tiefgang
Historisch gesehen war es üblich, die asynchrone Variante `fs.exists` zu verwenden, um die Blockierung des Event-Loops zu verhindern. Allerdings ist `fs.exists` nicht empfohlen, weil es veraltet ist und in zukünftigen Versionen entfernt werden könnte. Die Synchronversion `fs.existsSync` wird bevorzugt, wenn einfaches Blocking kein Problem ist (z.B. beim initialen Setup).

Alternativ kann `fs.access` mit `fs.constants.F_OK` verwendet werden, um zu prüfen, ob ein Pfad zugänglich ist, was impliziert, dass das Verzeichnis existiert:

```typescript
import { access, constants } from 'fs';

access(directoryPath, constants.F_OK, (err) => {
    if (err) {
        console.log('Das Verzeichnis existiert nicht.');
    } else {
        console.log('Das Verzeichnis existiert.');
    }
});
```

Implementationstechnisch arbeitet `fs.existsSync` direkt mit einer systemnahen API, um Dateiattribute zu überprüfen. Das ist effizient, weil es unnötige Overheads wie Promises oder Callbacks vermeidet.

## Siehe Auch
- Node.js `fs` Dokumentation: https://nodejs.org/api/fs.html
- TypeScript Handbuch: https://www.typescriptlang.org/docs/
- Artikel über den Unterschied zwischen sync- und async-Methoden in Node.js: https://nodejs.org/en/docs/guides/blocking-vs-non-blocking/