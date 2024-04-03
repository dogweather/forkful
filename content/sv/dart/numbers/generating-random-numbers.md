---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:42.714077-07:00
description: "Att generera slumpm\xE4ssiga tal i Dart involverar att skapa numeriska\
  \ v\xE4rden som \xE4r of\xF6ruts\xE4gbara och skiljer sig \xE5t vid varje k\xF6\
  rning. Programmerare\u2026"
lastmod: '2024-03-13T22:44:37.607423-06:00'
model: gpt-4-0125-preview
summary: "Att generera slumpm\xE4ssiga tal i Dart involverar att skapa numeriska v\xE4\
  rden som \xE4r of\xF6ruts\xE4gbara och skiljer sig \xE5t vid varje k\xF6rning."
title: "Generera slumpm\xE4ssiga nummer"
weight: 12
---

## Hur man gör:
Darts kärnbibliotek inkluderar stöd för att generera slumpmässiga tal med klassen `Random` som finns i `dart:math`. Här är ett grundläggande exempel:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int slumpmässigtTal = rand.nextInt(100); // Genererar ett slumpmässigt heltal mellan 0 och 99
  double slumpmässigtDubbel = rand.nextDouble(); // Genererar en slumpmässig dubbel mellan 0.0 och 1.0
  print(slumpmässigtTal);
  print(slumpmässigtDubbel);
}
```

*Exempelutdata: (Detta kommer variera varje gång det körs)*

```
23
0.6722390975465775
```

För användningsområden som kräver kryptografisk slumpmässighet, erbjuder Dart konstruktören `Random.secure`:

```dart
import 'dart:math';

void main() {
  var säkerRand = Random.secure();
  int säkertSlumpmässigtTal = säkerRand.nextInt(100);
  print(säkertSlumpmässigtTal);
}
```

*Exempelutdata: (Detta kommer variera varje gång det körs)*

```
45
```

Om du arbetar med Flutter-projekt eller behöver mer komplex slumpmässighet, kan du hitta paketet `faker` användbart för att generera ett brett utbud av slumpmässiga data, såsom namn, adresser och datum.

För att använda `faker`, lägg först till det i din `pubspec.yaml`-fil:

```yaml
dependencies:
  faker: ^2.0.0
```

Importera sedan och använd det som visat:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Genererar ett slumpmässigt namn
  print(faker.address.city()); // Genererar ett slumpmässigt stadsnamn
}
```

*Exempelutdata:*

```
Josie Runolfsdottir
East Lysanne
```
