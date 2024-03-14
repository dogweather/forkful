---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:51.249184-07:00
description: "Eine interaktive Shell (REPL - Read-Evaluate-Print Loop) f\xFCr Dart\
  \ erm\xF6glicht es Programmierern, Dart-Code Zeile f\xFCr Zeile dynamisch zu tippen\
  \ und\u2026"
lastmod: '2024-03-13T22:44:53.582779-06:00'
model: gpt-4-0125-preview
summary: "Eine interaktive Shell (REPL - Read-Evaluate-Print Loop) f\xFCr Dart erm\xF6\
  glicht es Programmierern, Dart-Code Zeile f\xFCr Zeile dynamisch zu tippen und\u2026"
title: Verwendung einer interaktiven Shell (REPL)
---

{{< edit_this_page >}}

## Was & Warum?

Eine interaktive Shell (REPL - Read-Evaluate-Print Loop) für Dart ermöglicht es Programmierern, Dart-Code Zeile für Zeile dynamisch zu tippen und auszuführen, ohne ganze Skripte kompilieren zu müssen. Dieses Werkzeug ist unschätzbar wertvoll, um die Syntax von Dart zu erlernen, mit Code-Schnipseln zu experimentieren oder zu debuggen, indem es sofortiges Feedback bietet und iteratives Testen erleichtert.

## Wie:

Dart kommt nicht mit einem eingebauten REPL. Dennoch können Sie eine REPL-ähnliche Funktionalität erreichen, indem Sie DartPad (online) verwenden oder Drittanbieter-Tools wie `dart_repl` nutzen.

**DartPad verwenden:**

DartPad (https://dartpad.dev) ist ein Online-Dart-Editor, mit dem Sie Dart-Code in Ihrem Webbrowser schreiben und ausführen können. Obwohl es kein traditionelles Befehlszeilen-REPL ist, bietet es eine ähnliche Erfahrung für schnelles Experimentieren.

Gehen Sie einfach auf die Website, tippen Sie Ihren Dart-Code in das linke Fenster und klicken Sie auf "Run", um die Ausgabe auf der rechten Seite zu sehen.

Beispiel:
```dart
void main() {
  print('Hallo, Dart!');
}
```
Ausgabe:
```
Hallo, Dart!
```

**`dart_repl` verwenden (Drittanbieter-Tool):**

Installieren Sie zuerst `dart_repl` global über pub:

```shell
dart pub global activate dart_repl
```

Führen Sie dann `dart_repl` von Ihrem Terminal aus:

```shell
dart_repl
```

Nun können Sie direkt in die Shell Dart-Anweisungen tippen. Zum Beispiel:

```dart
>>> print('Hallo, REPL!');
Hallo, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Diese Methoden bieten einen schnellen Weg, um Dart-Code spontan auszuprobieren, was die Lernkurve erheblich erleichtert und die Produktivität steigert.
