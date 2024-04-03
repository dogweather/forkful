---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:40.103020-07:00
description: "Das Generieren von Zufallszahlen in Dart beinhaltet die Erstellung von\
  \ numerischen Werten, die unvorhersehbar sind und bei jeder Ausf\xFChrung variieren.\u2026"
lastmod: '2024-03-13T22:44:53.576067-06:00'
model: gpt-4-0125-preview
summary: "Das Generieren von Zufallszahlen in Dart beinhaltet die Erstellung von numerischen\
  \ Werten, die unvorhersehbar sind und bei jeder Ausf\xFChrung variieren."
title: Zufallszahlen generieren
weight: 12
---

## Was & Warum?
Das Generieren von Zufallszahlen in Dart beinhaltet die Erstellung von numerischen Werten, die unvorhersehbar sind und bei jeder Ausführung variieren. Programmierer nutzen diese Funktionalität aus verschiedenen Gründen, von der Simulation realer Szenarien in Testumgebungen bis hin zur Aktivierung von Spielmechaniken und der Gewährleistung von Sicherheit durch Zufälligkeit in kryptografischen Operationen.

## Wie geht das:

Die Kernbibliothek von Dart unterstützt das Generieren von Zufallszahlen mit der `Random`-Klasse, die in `dart:math` gefunden werden kann. Hier ist ein einfaches Beispiel:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Erzeugt eine zufällige Ganzzahl zwischen 0 und 99
  double randomDouble = rand.nextDouble(); // Erzeugt eine zufällige Fließkommazahl zwischen 0.0 und 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Beispielausgabe: (Diese variiert bei jedem Ausführen)*

```
23
0.6722390975465775
```

Für Anwendungsfälle, die kryptografische Zufälligkeit erfordern, bietet Dart den Konstruktor `Random.secure` an:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Beispielausgabe: (Diese variiert bei jedem Ausführen)*

```
45
```

Wenn Sie an Flutter-Projekten arbeiten oder komplexere Zufälligkeit benötigen, finden Sie möglicherweise das `faker`-Paket nützlich, um eine breite Palette von zufälligen Daten zu generieren, wie z. B. Namen, Adressen und Daten.

Um `faker` zu verwenden, fügen Sie es zunächst Ihrer `pubspec.yaml`-Datei hinzu:

```yaml
dependencies:
  faker: ^2.0.0
```

Dann importieren und verwenden Sie es wie gezeigt:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Erzeugt einen zufälligen Namen
  print(faker.address.city()); // Erzeugt einen zufälligen Stadtnamen
}
```

*Beispielausgabe:*

```
Josie Runolfsdottir
East Lysanne
```
