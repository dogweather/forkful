---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:38.790335-07:00
description: "Generar n\xFAmeros aleatorios en Dart implica crear valores num\xE9\
  ricos que son impredecibles y difieren en cada ejecuci\xF3n. Los programadores aprovechan\
  \ esta\u2026"
lastmod: '2024-03-13T22:44:58.745399-06:00'
model: gpt-4-0125-preview
summary: "Generar n\xFAmeros aleatorios en Dart implica crear valores num\xE9ricos\
  \ que son impredecibles y difieren en cada ejecuci\xF3n."
title: "Generando n\xFAmeros aleatorios"
weight: 12
---

## Qué & Por qué?
Generar números aleatorios en Dart implica crear valores numéricos que son impredecibles y difieren en cada ejecución. Los programadores aprovechan esta funcionalidad por una variedad de razones, desde simular escenarios del mundo real en entornos de prueba hasta habilitar mecánicas de juego y asegurar la seguridad a través de la aleatoriedad en operaciones criptográficas.

## Cómo hacerlo:

La biblioteca central de Dart incluye soporte para generar números aleatorios con la clase `Random` encontrada en `dart:math`. Aquí hay un ejemplo básico:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Genera un entero aleatorio entre 0 y 99
  double randomDouble = rand.nextDouble(); // Genera un doble aleatorio entre 0.0 y 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Salida de muestra: (Esto variará cada vez que se ejecute)*

```
23
0.6722390975465775
```

Para casos de uso que requieren aleatoriedad criptográfica, Dart ofrece el constructor `Random.secure`:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Salida de muestra: (Esto variará cada vez que se ejecute)*

```
45
```

Si estás trabajando en proyectos de Flutter o necesitas una aleatoriedad más compleja, podrías encontrar útil el paquete `faker` para generar una amplia gama de datos aleatorios, como nombres, direcciones y fechas.

Para usar `faker`, primero, agrégalo a tu archivo `pubspec.yaml`:

```yaml
dependencies:
  faker: ^2.0.0
```

Luego, impórtalo y úsalo como se muestra:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Genera un nombre aleatorio
  print(faker.address.city()); // Genera un nombre de ciudad aleatorio
}
```

*Salida de muestra:*

```
Josie Runolfsdottir
East Lysanne
```
