---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:44.413329-07:00
description: "Die Verwendung eines Debuggers in Dart erm\xF6glicht es Programmierern,\
  \ ihren Code systematisch zu \xFCberpr\xFCfen, indem sie Haltepunkte setzen, die\
  \ Ausf\xFChrung\u2026"
lastmod: '2024-03-09T21:06:17.580122-07:00'
model: gpt-4-0125-preview
summary: "Die Verwendung eines Debuggers in Dart erm\xF6glicht es Programmierern,\
  \ ihren Code systematisch zu \xFCberpr\xFCfen, indem sie Haltepunkte setzen, die\
  \ Ausf\xFChrung\u2026"
title: Einen Debugger verwenden
---

{{< edit_this_page >}}

## Was & Warum?

Die Verwendung eines Debuggers in Dart ermöglicht es Programmierern, ihren Code systematisch zu überprüfen, indem sie Haltepunkte setzen, die Ausführung Schritt für Schritt durchgehen und Variablen inspizieren. Dieser Prozess ist essentiell, um Bugs effizient zu identifizieren und zu beheben, und macht ihn somit zu einem unverzichtbaren Werkzeug im Entwicklungszyklus.

## Wie geht das:

### Grundlegendes Debugging:

**1. Haltepunkte setzen:**

Um einen Haltepunkt zu setzen, klicken Sie einfach auf den linken Rand der Codezeile in Ihrer IDE (z. B. Visual Studio Code oder Android Studio), an der die Ausführung pausieren soll.

```dart
void main() {
  var message = 'Hallo, Debugging';
  print(message); // Setze hier einen Haltepunkt
}
```

**2. Debugging starten:**

Initiieren Sie in Ihrer IDE eine Debuggingsitzung, indem Sie auf das Debug-Symbol klicken oder die Debug-Taste drücken. Die Ausführung wird an Haltepunkten pausieren.

**3. Variablen inspizieren:**

Sobald die Ausführung pausiert ist, fahren Sie mit der Maus über Variablen, um deren aktuelle Werte zu sehen.

**4. Schrittweise durch den Code gehen:**

Verwenden Sie die Befehle "Step Over", "Step Into" und "Step Out" in Ihrer IDE, um Schritt für Schritt durch Ihren Code, eine Zeile oder Funktion nach der anderen, zu navigieren.

### Fortgeschrittenes Debugging mit Observatory:

Dart beinhaltet ein Werkzeug namens Observatory für das Debugging und Profiling von Dart-Anwendungen. Es ist besonders nützlich für Anwendungen, die auf der Dart VM laufen.

**Observatory zugreifen:**

Führen Sie Ihre Dart-Anwendung mit dem `--observe` Flag aus.

```bash
dart --observe Ihr_Programm.dart
```

Dieser Befehl druckt eine URL in die Konsole, die Sie in einem Webbrowser öffnen können, um auf den Observatory Debugger zuzugreifen.

### Verwendung beliebter Drittanbieter-Bibliotheken:

Für das Debugging von Flutter-Anwendungen bietet das `flutter_devtools` Paket eine Reihe von Leistungs- und Debugging-Tools, die sowohl mit der Dart VM als auch mit Flutter integrieren.

**Installation:**

Fügen Sie zuerst `devtools` Ihrer `pubspec.yaml`-Datei unter `dev_dependencies` hinzu:

```yaml
dev_dependencies:
  devtools: any
```

**DevTools starten:**

Führen Sie diesen Befehl in Ihrem Terminal aus:

```bash
flutter pub global run devtools
```

Starten Sie dann Ihre Flutter-Anwendung im Debug-Modus. DevTools bietet Funktionen wie den Flutter-Inspektor zur Analyse des Widget-Baums und den Netzwerk-Profilierer zur Überwachung der Netzwerkaktivität.

### Beispiel-Ausgabe:

Wenn ein Haltepunkt erreicht wird, könnte Ihre IDE Variablenwerte und Stack-Trace wie folgt anzeigen:

```
message: 'Hallo, Debugging'
```

Indem Entwickler Debugging-Tools und -Techniken in Dart effektiv nutzen, können sie Probleme schneller identifizieren und lösen, was zu einem reibungsloseren Entwicklungsprozess und robusteren Anwendungen führt.
