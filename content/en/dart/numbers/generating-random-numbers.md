---
date: 2024-03-08 21:33:41.071470-07:00
description: "Generating random numbers in Dart involves creating numerical values\
  \ that are unpredictable and differ on each execution. Programmers leverage this\u2026"
lastmod: '2024-03-11T00:14:33.673823-06:00'
model: gpt-4-0125-preview
summary: "Generating random numbers in Dart involves creating numerical values that\
  \ are unpredictable and differ on each execution. Programmers leverage this\u2026"
title: Generating random numbers
---

{{< edit_this_page >}}

## What & Why?
Generating random numbers in Dart involves creating numerical values that are unpredictable and differ on each execution. Programmers leverage this functionality for a variety of reasons, from simulating real-world scenarios in testing environments to enabling game mechanics and ensuring security through randomness in cryptographic operations.

## How to:

Dart's core library includes support for generating random numbers with the `Random` class found in `dart:math`. Here's a basic example:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Generates a random integer between 0 and 99
  double randomDouble = rand.nextDouble(); // Generates a random double between 0.0 and 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Sample output: (This will vary each time it is run)*

```
23
0.6722390975465775
```

For use cases requiring cryptographic randomness, Dart offers the `Random.secure` constructor:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Sample output: (This will vary each time it is run)*

```
45
```

If you're working on Flutter projects or need more complex randomness, you might find the `faker` package useful for generating a wide range of random data, such as names, addresses, and dates. 

To use `faker`, first, add it to your `pubspec.yaml` file:

```yaml
dependencies:
  faker: ^2.0.0
```

Then, import and use it as shown:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Generates a random name
  print(faker.address.city()); // Generates a random city name
}
```

*Sample output:*

```
Josie Runolfsdottir
East Lysanne
```
