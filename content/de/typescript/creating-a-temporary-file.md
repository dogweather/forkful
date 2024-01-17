---
title:                "Erstellen einer temporären Datei"
html_title:           "TypeScript: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Was & Warum?

Die Erstellung von temporären Dateien ist ein gängiges Konzept in der Programmierung, bei dem eine Datei dynamisch im Speicher erstellt wird, um temporäre Daten zu speichern. Programmierer nutzen diese Technik, um schnell auf temporäre Daten zuzugreifen oder um sicherzustellen, dass die Daten nicht dauerhaft im Speicher vorhanden sind, was Speicherplatz spart.

# Wie geht's?

```TypeScript
const fs = require('fs');
const path = require('path');

// Erstellen Sie eine temporäre Datei im Speicher
const tempFile = fs.createTempFile();

// Schreiben Sie Daten in die temporäre Datei
fs.writeFileSync(tempFile, 'Diese Daten sind temporär');

// Lesen Sie Daten aus der temporären Datei
const data = fs.readFileSync(tempFile);

// Löschen der temporären Datei
fs.unlinkSync(tempFile);

// Output:
// Diese Daten sind temporär

```

# Tiefer tauchen

Die Erstellung von temporären Dateien hat eine lange Geschichte in der Programmierung, insbesondere in Betriebssystemen, die nicht über eingebaute Speicher-Management-Funktionen verfügen. Eine Alternative zur Verwendung von temporären Dateien ist die Verwendung von temporären Variablen im Speicher, bei denen die Daten direkt gespeichert und anschließend überschrieben werden, anstatt sie in einer Datei zu speichern. Die Implementierung von temporären Dateien kann je nach Betriebssystem variieren, wobei einige spezielle Funktionen oder Bibliotheken erfordern.

# Siehe auch

Weitere Informationen zur Verwendung von temporären Dateien in TypeScript finden Sie in der offiziellen Dokumentation unter: https://www.typescriptlang.org/docs/handbook/deno-api.html#gdrw0009. Alternativ können Sie auch die Funktionen von Node.js, einer populären JavaScript-Plattform, nutzen, da es auch die Erstellung und Verwaltung von temporären Dateien unterstützt. Weitere Informationen dazu finden Sie unter: https://nodejs.org/api/fs.html#fs_fs_createtempfile_options_callback.