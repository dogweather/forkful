---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Anlegen einer temporären Datei bedeutet das Erstellen einer Datei, die nur für die Dauer einer Session oder eines Prozesses existiert. Programmierer tun dies normalerweise für die Zwischenspeicherung von Daten oder zum Testen von Code ohne die eigentlichen Dateien zu beeinflussen.

## So geht's:

Dafür nutzen wir das `fs` Modul in Node.js. Hier ist ein einfacher TypeScript-Code, der eine temporäre Datei erstellt:

```TypeScript
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

// Temporäre Datei erstellen
let tempDir = os.tmpdir();
let tempFile = path.join(tempDir, 'temp.txt');

fs.writeFileSync(tempFile, 'Dies ist eine temporäre Datei');
console.log('Temporäre Datei erstellt unter:', tempFile);
```

Ausführung des obigen Skripts wird diese Ausgabe geben:

```Shell
Temporäre Datei wurde unter erstellt: /tmp/temp.txt
```

## Tiefer eintauchen:

Temporäre Dateien sind nicht neu; sie wurden in frühen Betriebssystemen verwendet, um Speicherplatz zu sparen oder um Daten zwischen Anwendungen auszutauschen. Es gibt verschiedene Wege, eine temporäre Datei zu erstellen. Manche benutzen Drittanbieter-Bibliotheken wie `tmp-promise`. Andere gehen den nativen Weg mit dem eingebauten `os` und `fs` Modulen, wie in unserem Beispiel zuvor.

Implementierungsdetails können je nach Anforderung variieren. Man könnte z.B. eine Datei mit zufällig generiertem Namen erstellen, um Kollisionen zu vermeiden. Außerdem kann man die Datei gleichzeitig mit Daten beschreiben oder sie zuerst leer erstellen und später befüllen.

## Siehe auch:

1. Node.js Dokumentation für das `fs` Modul: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)

2. TypeScript Dokumentation: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)

3. NPM-Paket für `tmp-promise`: [https://www.npmjs.com/package/tmp-promise](https://www.npmjs.com/package/tmp-promise) 

Viel Spaß beim Programmieren!