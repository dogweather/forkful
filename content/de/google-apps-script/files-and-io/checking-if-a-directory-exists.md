---
title:                "Überprüfung, ob ein Verzeichnis existiert"
aliases: - /de/google-apps-script/checking-if-a-directory-exists.md
date:                  2024-02-01T21:48:56.631355-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/google-apps-script/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis in Google Apps Script existiert, beinhaltet die Verifizierung der Präsenz eines Ordners innerhalb von Google Drive. Programmierer führen diese Überprüfung oft durch, um Fehler oder die redundante Erstellung von Ordnern zu vermeiden, wenn sie Dateien und Verzeichnisse programmatisch verwalten.

## Wie:

Google Apps Script bietet keine direkte "exists"-Methode für Ordner. Stattdessen verwenden wir die Suchfunktionen von Google Drive, um zu überprüfen, ob ein Ordner mit einem bestimmten Namen existiert. Hier ist ein schrittweises Beispiel:

```javascript
// Funktion, um zu überprüfen, ob ein Verzeichnis existiert
function checkIfDirectoryExists(directoryName) {
  // Die Sammlung von Ordnern mit dem angegebenen Namen abrufen
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // Überprüfen, ob mindestens ein Ordner mit dem angegebenen Namen existiert
  if (folders.hasNext()) {
    Logger.log('Verzeichnis existiert.');
    return true;
  } else {
    Logger.log('Verzeichnis existiert nicht.');
    return false;
  }
}

// Beispielverwendung
var directoryName = 'Mein Beispielordner';
checkIfDirectoryExists(directoryName);
```

Beispielausgabe:
```
Verzeichnis existiert.
```
oder 
```
Verzeichnis existiert nicht.
```

Dieses Skript nutzt die Methode `getFoldersByName`, die alle Ordner im Drive des Benutzers abruft, die dem angegebenen Namen entsprechen. Da Namen in Drive nicht einzigartig sind, gibt diese Methode einen `FolderIterator` zurück. Die Präsenz eines nächsten Elements (`hasNext()`) in diesem Iterator zeigt an, dass das Verzeichnis existiert.

## Vertiefung

Historisch gesehen hat sich die Dateiverwaltung in Web- und Cloud-Umgebungen signifikant weiterentwickelt. Google Apps Script, das eine umfangreiche API für Google Drive bietet, ermöglicht komplexe Datei- und Ordnerverwaltungsoperationen, einschließlich der dargestellten Such- und Überprüfungsmechanismen. Ein bemerkenswerter Aspekt ist jedoch das Fehlen einer direkten Existenzprüfung, wahrscheinlich aufgrund der Erlaubnis von Google Drive für mehrere Ordner mit demselben Namen, was im Kontrast zu vielen Dateisystemen steht, die eindeutige Namen innerhalb desselben Verzeichnisses erzwingen.

In diesem Kontext ist die Verwendung der Methode `getFoldersByName` ein wirksamer Behelf, könnte jedoch in einem Szenario, in dem eine große Anzahl von Ordnern mit doppelten Namen existiert, potenziell Ineffizienzen einführen. Ein alternativer Ansatz könnte darin bestehen, eine anwendungsspezifische Indexierung oder Benennungskonvention zu pflegen, um schnellere Überprüfungen zu gewährleisten, insbesondere wenn die Leistung zu einem kritischen Anliegen wird.

Obwohl der Ansatz von Google Apps Script im Vergleich zu Dateiexistenzüberprüfungen in Programmiersprachen, die direkt mit einem einzelnen Dateisystem interagieren, zunächst weniger direkt erscheinen mag, spiegelt er die Notwendigkeit wider, die Komplexitäten der Cloud-basierten Dateispeicherung zu handhaben. Entwickler, die Google Apps Script für die Verwaltung von Drive nutzen, sollten diese Nuancen berücksichtigen und für die Stärken und Einschränkungen von Google Drive optimieren.
