---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:08.357079-07:00
description: "Refactoring in Dart ist der Prozess des Umstrukturierens bestehenden\
  \ Codes, ohne dessen externes Verhalten zu \xE4ndern, mit dem Ziel, seine interne\u2026"
lastmod: '2024-03-09T21:06:17.583731-07:00'
model: gpt-4-0125-preview
summary: "Refactoring in Dart ist der Prozess des Umstrukturierens bestehenden Codes,\
  \ ohne dessen externes Verhalten zu \xE4ndern, mit dem Ziel, seine interne\u2026"
title: Refaktorisierung
---

{{< edit_this_page >}}

## Was & Warum?

Refactoring in Dart ist der Prozess des Umstrukturierens bestehenden Codes, ohne dessen externes Verhalten zu ändern, mit dem Ziel, seine interne Struktur, Lesbarkeit und Wartbarkeit zu verbessern. Programmierer führen oft ein Refactoring durch, um den Code sauberer, leichter verständlich oder effizienter zu machen, wodurch zukünftige Änderungen erleichtert und die Wahrscheinlichkeit von Fehlern verringert wird.

## Wie geht das:

### Beispiel 1: Umbenennen und Extrahieren von Methoden

Vor dem Refactoring haben Sie möglicherweise einen Codeabschnitt, der verschiedene Abstraktionsebenen oder Verantwortlichkeiten mischt, wie zum Beispiel das Berechnen eines Rabatts und dann dessen Anwendung:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Endpreis: $finalPrice");
}
```

**Ausgabe:**
```
Endpreis: 80.0
```

Nach dem Refactoring können Sie die Rabattberechnung in eine eigene Methode extrahieren und ihr einen aussagekräftigen Namen geben:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("Endpreis: $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**Ausgabe:**
```
Endpreis: 80.0
```

Indem Sie die Berechnung in eine Methode extrahieren, haben Sie jetzt eine klar definierte Operation, die wiederverwendet, unabhängig getestet und leicht modifiziert werden kann.

### Beispiel 2: Vereinfachung von Bedingten Ausdrücken

Vor dem Refactoring könnten bedingte Anweisungen übermäßig komplex oder schwer zu lesen sein:

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("Rabatt: $discount");
}
```

**Ausgabe:**
```
Rabatt: 0.05
```

Nach dem Refactoring sollten Sie erwägen, eine Map für eine klarere Struktur und einfachere Updates oder Erweiterungen von Kundentypen und Rabatten zu verwenden:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Rabatt: $discount");
}
```

**Ausgabe:**
```
Rabatt: 0.05
```

Dieses Refactoring macht den Code nicht nur prägnanter, sondern kapselt auch die Logik zur Bestimmung von Rabatten auf eine Weise ein, die leichter zu verstehen und zu warten ist.

### Drittanbieter-Bibliotheken für Refactoring

Wenn es um Refactoring in Dart geht, besonders innerhalb von Flutter-Apps, ist die [Dart DevTools](https://dart.dev/tools/dart-devtools)-Suite unschätzbar. Sie umfasst Leistungswerkzeuge, einen Widget-Inspektor und einen Quellcode-Debugger. Obwohl es sich nicht um eine Drittanbieter-Bibliothek handelt, werden Dart DevTools oft zusammen mit Bibliotheken wie `flutter_bloc` verwendet, um den Zustand auf eine Weise zu verwalten, die ein Refactoring zur Verbesserung der Modularität und Lesbarkeit fördert. Aufgrund des Umfangs dieses Eintrags werden hier keine spezifischen Codebeispiele mit Drittanbieter-Bibliotheken bereitgestellt, aber Entwickler werden ermutigt, diese Werkzeuge zu erkunden, um den Refactoring-Prozess in ihren Dart/Flutter-Anwendungen zu verbessern.
