---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:52.688632-07:00
description: "Hvordan: Darts kjernebibliotek inkluderer st\xF8tte for \xE5 generere\
  \ tilfeldige tall med `Random`-klassen funnet i `dart:math`. Her er et grunnleggende\u2026"
lastmod: '2024-03-13T22:44:40.482668-06:00'
model: gpt-4-0125-preview
summary: "Darts kjernebibliotek inkluderer st\xF8tte for \xE5 generere tilfeldige\
  \ tall med `Random`-klassen funnet i `dart:math`."
title: Genererer tilfeldige tall
weight: 12
---

## Hvordan:
Darts kjernebibliotek inkluderer støtte for å generere tilfeldige tall med `Random`-klassen funnet i `dart:math`. Her er et grunnleggende eksempel:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Genererer et tilfeldig heltall mellom 0 og 99
  double randomDouble = rand.nextDouble(); // Genererer et tilfeldig desimaltall mellom 0.0 og 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Eksempel på utskrift: (Dette vil variere hver gang det kjøres)*

```
23
0.6722390975465775
```

For brukstilfeller som krever kryptografisk tilfeldighet, tilbyr Dart `Random.secure`-konstruktøren:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Eksempel på utskrift: (Dette vil variere hver gang det kjøres)*

```
45
```

Hvis du jobber med Flutter-prosjekter eller trenger mer kompleks tilfeldighet, kan du finne `faker`-pakken nyttig for å generere et bredt spekter av tilfeldige data, som navn, adresser og datoer.

For å bruke `faker`, legg den først til i `pubspec.yaml`-filen din:

```yaml
dependencies:
  faker: ^2.0.0
```

Deretter importerer og bruker du den slik:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Genererer et tilfeldig navn
  print(faker.address.city()); // Genererer et tilfeldig bynavn
}
```

*Eksempel på utskrift:*

```
Josie Runolfsdottir
East Lysanne
```
