---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:47.325517-07:00
description: "Das Umwandeln eines Strings in Kleinbuchstaben ist eine grundlegende\
  \ Operation, die das Transformieren aller Zeichen eines gegebenen Strings in ihre\u2026"
lastmod: '2024-03-09T21:06:17.562904-07:00'
model: gpt-4-0125-preview
summary: "Das Umwandeln eines Strings in Kleinbuchstaben ist eine grundlegende Operation,\
  \ die das Transformieren aller Zeichen eines gegebenen Strings in ihre\u2026"
title: Eine Zeichenkette in Kleinbuchstaben umwandeln
---

{{< edit_this_page >}}

## Was & Warum?

Das Umwandeln eines Strings in Kleinbuchstaben ist eine grundlegende Operation, die das Transformieren aller Zeichen eines gegebenen Strings in ihre Kleinbuchstaben-Äquivalente beinhaltet. Programmierer führen diese Operation typischerweise durch, um Groß-/Kleinschreibung-unabhängige Vergleiche zu erreichen oder Texteingaben für die weitere Verarbeitung zu standardisieren. Damit werden Anwendungen benutzerfreundlicher und Daten konsistenter.

## Wie:

In Dart können Sie einen String mit der Methode `toLowerCase()` der `String`-Klasse in Kleinbuchstaben umwandeln. Diese Methode gibt einen neuen String zurück, bei dem alle Großbuchstaben in Kleinbuchstaben umgewandelt wurden. Sehen wir uns an, wie dies mit einem einfachen Beispiel funktioniert:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Ausgabe: hello, world!
}
```

Dart benötigt für grundlegende String-Manipulationsaufgaben, einschließlich der Umwandlung in Kleinbuchstaben, keine externen Bibliotheken, da die `String`-Klasse der Standardbibliothek ziemlich umfassend ist. Für komplexere Manipulationen, die lokal-spezifische Regeln beinhalten, könnte jedoch das `intl`-Paket in Betracht gezogen werden, das Internationalisierungs- und Lokalisierungsfunktionen bietet, einschließlich der Fallumwandlung basierend auf Lokaleinstellungen:

Um `intl` zu verwenden, fügen Sie es Ihrer `pubspec.yaml`-Datei hinzu:

```yaml
dependencies:
  intl: ^0.17.0
```

Anschließend können Sie die Methode `toLocaleLowerCase()` verwenden, um einen String basierend auf spezifischen Lokalen in Kleinbuchstaben umzuwandeln:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Türkisches Lokal
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Ausgabe: istanbul
  
  // Standardlokal (en)
  print(originalString.toLowerCase()); // Ausgabe: i̇stanbul
}
```

In diesem Beispiel beachten Sie, wie das türkische Lokal korrekt den punktlosen 'i' behandelt, was die Bedeutung lokalbewusster Transformationen in internationalisierten Anwendungen unterstreicht.
