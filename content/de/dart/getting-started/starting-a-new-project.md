---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:29.278396-07:00
description: "Ein neues Projekt in Dart zu starten, erfordert das Einrichten einer\
  \ Umgebung, die eine effiziente Entwicklung, Tests und Bereitstellung f\xF6rdert.\u2026"
lastmod: '2024-03-09T21:06:17.576198-07:00'
model: gpt-4-0125-preview
summary: "Ein neues Projekt in Dart zu starten, erfordert das Einrichten einer Umgebung,\
  \ die eine effiziente Entwicklung, Tests und Bereitstellung f\xF6rdert.\u2026"
title: Ein neues Projekt starten
---

{{< edit_this_page >}}

## Was & Warum?

Ein neues Projekt in Dart zu starten, erfordert das Einrichten einer Umgebung, die eine effiziente Entwicklung, Tests und Bereitstellung fördert. Programmierer initiieren neue Dart-Projekte, um die optimale Leistung und das robuste Ökosystem von Dart zu nutzen, insbesondere für die Entwicklung von Web- und Mobile-Apps mit Frameworks wie Flutter.

## Wie geht das:

1. **Dart installieren**:
   Stellen Sie sicher, dass Dart auf Ihrem System installiert ist. Wenn nicht, können Sie es von [https://dart.dev/get-dart](https://dart.dev/get-dart) herunterladen. Verifizieren Sie die Installation mit:

   ```shell
   dart --version
   ```

2. **Ein neues Dart-Projekt erstellen**:
   Verwenden Sie die Dart-CLI, um ein neues Projekt zu generieren:

   ```shell
   dart create hello_dart
   ```

   Dieser Befehl erstellt ein neues Verzeichnis `hello_dart` mit einer einfachen Beispiel-Web- oder Konsolenanwendung, abhängig von Ihrer Auswahl.

3. **Die Projektstruktur untersuchen**:
   
   Navigieren Sie zu Ihrem Projektverzeichnis:

   ```shell
   cd hello_dart
   ```

   Ein typisches Dart-Projekt umfasst die folgenden Schlüsseldateien und -verzeichnisse:

   - `pubspec.yaml`: Konfigurationsdatei, die die Abhängigkeiten Ihres Projekts und SDK-Beschränkungen enthält.
   - `lib/`: Verzeichnis, in dem der Großteil des Dart-Codes liegt.
   - `test/`: Verzeichnis für Projektests.

4. **Abhängigkeiten hinzufügen**:
   Bearbeiten Sie `pubspec.yaml`, um Abhängigkeiten hinzuzufügen. Für Webprojekte sollten Sie `http` hinzufügen, ein beliebtes Paket für HTTP-Anfragen:

   ```yaml
   dependencies:
     flutter:
       sdk: flutter
     http: ^0.13.3
   ```

   Nach der Bearbeitung holen Sie die Abhängigkeiten:

   ```shell
   dart pub get
   ```

5. **Ihren ersten Dart-Code schreiben**:
   
   Im Verzeichnis `lib/` erstellen Sie eine neue Dart-Datei, `main.dart`, und fügen Sie einen einfachen Dart-Code hinzu:

   ```dart
   // Die Dart-Kernbibliothek importieren
   import 'dart:core';

   void main() {
     print('Hallo, Dart!');
   }
   ```

6. **Ihre Dart-Anwendung ausführen**:

   Führen Sie Ihr Dart-Programm aus mit:

   ```shell
   dart run
   ```

   Die Ausgabe sollte sein:

   ```
   Hallo, Dart!
   ```

Indem Sie diesen Schritten folgen, haben Sie erfolgreich ein neues Dart-Projekt gestartet, von der Installation bis zum Ausführen Ihres ersten Dart-Codes. Dieses grundlegende Wissen bereitet den Weg, um tiefer in das reiche Ökosystem von Dart und seine Fähigkeiten zur Entwicklung skalierbarer Anwendungen einzutauchen.
