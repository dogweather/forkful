---
title:                "Genererer tilfeldige tall"
date:                  2024-03-08T21:54:52.688632-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?
Generering av tilfeldige tall i Dart innebærer å skape numeriske verdier som er uforutsigbare og varierer ved hver kjøring. Programmerere utnytter denne funksjonaliteten av flere grunner, fra å simulere virkelige scenarioer i testmiljøer til å muliggjøre spillmekanikker og sikre sikkerhet gjennom tilfeldighet i kryptografiske operasjoner.

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
