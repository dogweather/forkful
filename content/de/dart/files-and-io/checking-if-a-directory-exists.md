---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:31.085389-07:00
description: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Dart existiert, dreht sich\
  \ darum, die Anwesenheit eines Verzeichnisses auf einem angegebenen Pfad im Dateisystem\
  \ zu\u2026"
lastmod: '2024-03-13T22:44:53.596937-06:00'
model: gpt-4-0125-preview
summary: "Das \xDCberpr\xFCfen, ob ein Verzeichnis in Dart existiert, dreht sich darum,\
  \ die Anwesenheit eines Verzeichnisses auf einem angegebenen Pfad im Dateisystem\
  \ zu verifizieren, bevor Operationen wie das Lesen oder Schreiben von Dateien durchgef\xFC\
  hrt werden."
title: "\xDCberpr\xFCfung, ob ein Verzeichnis existiert"
weight: 20
---

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis in Dart existiert, dreht sich darum, die Anwesenheit eines Verzeichnisses auf einem angegebenen Pfad im Dateisystem zu verifizieren, bevor Operationen wie das Lesen oder Schreiben von Dateien durchgeführt werden. Programmierer tun dies, um Fehler zu vermeiden, die auftreten, wenn versucht wird, auf Verzeichnisse zuzugreifen oder sie zu modifizieren, die nicht existieren.

## Wie:

Dart verwendet die `dart:io` Bibliothek, um mit Dateien und Verzeichnissen zu arbeiten. Hier ist eine einfache Methode, um zu überprüfen, ob ein Verzeichnis existiert:

```dart
import 'dart:io';

void main() {
  var directory = Directory('Pfad/zu/deinem/Verzeichnis');

  if (directory.existsSync()) {
    print('Verzeichnis existiert');
  } else {
    print('Verzeichnis existiert nicht');
  }
}
```
Beispielausgabe, falls das Verzeichnis existiert:
```
Verzeichnis existiert
```

Oder, falls es nicht existiert:
```
Verzeichnis existiert nicht
```

Um komplexere Szenarien zu behandeln, wie zum Beispiel eine asynchrone Überprüfung oder das Erstellen eines Verzeichnisses, falls es nicht existiert, könnten Sie den folgenden Ansatz verwenden:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('Pfad/zu/deinem/Verzeichnis');

  // Asynchron überprüfen, ob das Verzeichnis existiert
  var exists = await directory.exists();
  if (exists) {
    print('Verzeichnis existiert');
  } else {
    print('Verzeichnis existiert nicht, wird erstellt...');
    await directory.create(); // Das erstellt das Verzeichnis
    print('Verzeichnis erstellt');
  }
}
```

Beispielausgabe, falls das Verzeichnis nicht existierte und erstellt wurde:
```
Verzeichnis existiert nicht, wird erstellt...
Verzeichnis erstellt
```

Die integrierten Fähigkeiten von Dart sind normalerweise ausreichend, um mit Dateien und Verzeichnissen umzugehen, sodass Drittanbieterbibliotheken für diese Aufgabe normalerweise nicht notwendig sind. Jedoch können für komplexere Dateisystemoperationen Pakete wie `path` (zur plattformübergreifenden Pfadmanipulation) die `dart:io` Bibliothek ergänzen, bieten aber keine direkt fortgeschritteneren Überprüfungen der Existenz von Verzeichnissen an, als das, was gezeigt wurde.
