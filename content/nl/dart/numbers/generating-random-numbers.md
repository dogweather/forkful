---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:54.439984-07:00
description: "Het genereren van willekeurige getallen in Dart omvat het cre\xEBren\
  \ van numerieke waarden die onvoorspelbaar zijn en bij elke uitvoering verschillen.\u2026"
lastmod: '2024-03-13T22:44:50.500378-06:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen in Dart omvat het cre\xEBren van\
  \ numerieke waarden die onvoorspelbaar zijn en bij elke uitvoering verschillen."
title: Genereren van willekeurige getallen
weight: 12
---

## Hoe te:
De kernbibliotheek van Dart bevat ondersteuning voor het genereren van willekeurige getallen met de `Random` klasse, te vinden in `dart:math`. Hier is een eenvoudig voorbeeld:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int willekeurigGetal = rand.nextInt(100); // Genereert een willekeurig geheel getal tussen 0 en 99
  double willekeurigeDouble = rand.nextDouble(); // Genereert een willekeurige double tussen 0.0 en 1.0
  print(willekeurigGetal);
  print(willekeurigeDouble);
}
```

*Voorbeelduitvoer: (Dit zal elke keer dat het wordt uitgevoerd variëren)*

```
23
0.6722390975465775
```

Voor gebruiksscenario's die cryptografische willekeurigheid vereisen, biedt Dart de `Random.secure` constructor:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int veiligWillekeurigGetal = secureRand.nextInt(100);
  print(veiligWillekeurigGetal);
}
```

*Voorbeelduitvoer: (Dit zal elke keer dat het wordt uitgevoerd variëren)*

```
45
```

Als je werkt aan Flutter-projecten of meer complexe willekeurigheid nodig hebt, vind je het `faker` pakket wellicht nuttig voor het genereren van een breed scala aan willekeurige gegevens, zoals namen, adressen en datums.

Om `faker` te gebruiken, voeg je het eerst toe aan je `pubspec.yaml`-bestand:

```yaml
dependencies:
  faker: ^2.0.0
```

Importeer het vervolgens en gebruik het zoals getoond:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Genereert een willekeurige naam
  print(faker.address.city()); // Genereert een willekeurige stadsnaam
}
```

*Voorbeelduitvoer:*

```
Josie Runolfsdottir
East Lysanne
```
