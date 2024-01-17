---
title:                "Überprüfen ob ein Verzeichnis existiert"
html_title:           "TypeScript: Überprüfen ob ein Verzeichnis existiert"
simple_title:         "Überprüfen ob ein Verzeichnis existiert"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was ist das und Warum?

Das Überprüfen, ob ein Verzeichnis vorhanden ist, bedeutet, zu überprüfen, ob ein bestimmter Ordnernpfad auf Ihrem Computer existiert. Programmierer führen diese Überprüfung aus, um sicherzustellen, dass der Pfad korrekt ist und dass sie auf die Dateien innerhalb dieses Verzeichnisses zugreifen können.

## Wie man es macht:

```TypeScript
if (fs.existsSync(path)) {
  // Führe hier deine Aktionen aus, wenn das Verzeichnis existiert
} else {
  // Führe hier deine Aktionen aus, wenn das Verzeichnis nicht existiert
}
```

### Ausgabe-Beispiel:

Angenommen, wir überprüfen, ob das Verzeichnis "Dokumente" auf unserem Computer existiert:

```TypeScript
import * as fs from 'fs';
const path = 'C:/Benutzer/Dein_Benutzername/Dokumente';

if (fs.existsSync(path)) {
  console.log(`Das Verzeichnis ${path} existiert.`);
} else {
  console.log(`Das Verzeichnis ${path} existiert nicht.`);
}
```

Ausgabe:

Das Verzeichnis C:/Benutzer/Dein_Benutzername/Dokumente existiert.

## Tiefergehende Informationen:

### Historischer Kontext:

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Teil der Dateiverwaltung in Computersystemen. Es wurde in den frühen Tagen der Computertechnologie entwickelt und wird immer noch in aktuellen Programmiersprachen wie TypeScript verwendet.

### Alternativen:

Eine Alternative zur Überprüfung, ob ein Verzeichnis existiert, ist das Erstellen eines neuen Verzeichnisses, wenn es nicht vorhanden ist. Dies ist jedoch nicht immer die beste Lösung, da dies den Dateisystem-Workflow ändern kann.

### Implementierungsdetails:

Die `fs.existsSync()` Methode in TypeScript verwendet das File System Modul, um zu überprüfen, ob der angegebene Pfad ein gültiges Verzeichnis ist. Wenn dies der Fall ist, gibt sie `true` zurück, andernfalls `false`.

## Siehe auch:

[Das File System Modul in TypeScript](https://www.typescriptlang.org/docs/handbook/file-system.html).

[Wie man Verzeichnisse in TypeScript erstellt](https://www.digitalocean.com/community/tutorials/nodejs-working-with-folders).