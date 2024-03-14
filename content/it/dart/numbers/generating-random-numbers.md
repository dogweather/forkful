---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:33.066708-07:00
description: "Generare numeri casuali in Dart comporta la creazione di valori numerici\
  \ imprevedibili e diversi ad ogni esecuzione. I programmatori sfruttano questa\u2026"
lastmod: '2024-03-13T22:44:43.125173-06:00'
model: gpt-4-0125-preview
summary: "Generare numeri casuali in Dart comporta la creazione di valori numerici\
  \ imprevedibili e diversi ad ogni esecuzione. I programmatori sfruttano questa\u2026"
title: Generare numeri casuali
---

{{< edit_this_page >}}

## Cosa & Perché?
Generare numeri casuali in Dart comporta la creazione di valori numerici imprevedibili e diversi ad ogni esecuzione. I programmatori sfruttano questa funzionalità per vari motivi, dalla simulazione di scenari del mondo reale in ambienti di test, all'abilitazione di meccaniche di gioco e alla garanzia della sicurezza attraverso la casualità nelle operazioni crittografiche.

## Come fare:

La libreria core di Dart include il supporto per la generazione di numeri casuali con la classe `Random` trovata in `dart:math`. Ecco un esempio base:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int numeroCasuale = rand.nextInt(100); // Genera un intero casuale tra 0 e 99
  double doppioCasuale = rand.nextDouble(); // Genera un doppio casuale tra 0.0 e 1.0
  print(numeroCasuale);
  print(doppioCasuale);
}
```

*Output di esempio: (Questo varierà ogni volta che viene eseguito)*

```
23
0.6722390975465775
```

Per casi d'uso che richiedono casualità crittografica, Dart offre il costruttore `Random.secure`:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int numeroCasualeSicuro = secureRand.nextInt(100);
  print(numeroCasualeSicuro);
}
```

*Output di esempio: (Questo varierà ogni volta che viene eseguito)*

```
45
```

Se stai lavorando a progetti Flutter o hai bisogno di casualità più complesse, potresti trovare utile il pacchetto `faker` per generare un'ampia gamma di dati casuali, come nomi, indirizzi e date.

Per usare `faker`, prima aggiungilo al tuo file `pubspec.yaml`:

```yaml
dependencies:
  faker: ^2.0.0
```

Poi, importalo e usalo come mostrato:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Genera un nome casuale
  print(faker.address.city()); // Genera un nome di città casuale
}
```

*Output di esempio:*

```
Josie Runolfsdottir
East Lysanne
```
