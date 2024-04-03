---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:33.733974-07:00
description: "Calcolare una data nel futuro o nel passato \xE8 un compito comune per\
  \ i programmatori, che si occupano di pianificazioni, promemoria o di qualsiasi\
  \ funzione\u2026"
lastmod: '2024-03-13T22:44:43.146610-06:00'
model: gpt-4-0125-preview
summary: "Calcolare una data nel futuro o nel passato \xE8 un compito comune per i\
  \ programmatori, che si occupano di pianificazioni, promemoria o di qualsiasi funzione\
  \ che dipenda dai calcoli sulle date."
title: Calcolare una data nel futuro o nel passato
weight: 26
---

## Come fare:
Dart fornisce un robusto supporto per la manipolazione delle date attraverso la sua classe `DateTime`. Ecco come puoi calcolare date future o passate usando Dart nativo, senza la necessità di librerie di terze parti.

### Calcolare una Data Futura
Per calcolare una data futura, crei un oggetto `DateTime` e usi il metodo `add` con la durata desiderata.

```dart
DateTime oggi = DateTime.now();
Duration dieciGiorni = Duration(days: 10);
DateTime dataFutura = oggi.add(dieciGiorni);

print(dataFutura); // Output: 2023-04-21 14:22:35.123456 (output di esempio, dipende dalla data e dall'ora correnti)
```

### Calcolare una Data Passata
Per calcolare una data nel passato, usi il metodo `subtract` su un oggetto `DateTime` con la durata necessaria.

```dart
DateTime oggi = DateTime.now();
Duration quindiciGiorniFa = Duration(days: 15);
DateTime dataPassata = oggi.subtract(quindiciGiorniFa);

print(dataPassata); // Output: 2023-03-27 14:22:35.123456 (output di esempio, dipende dalla data e dall'ora correnti)
```

### Usare Librerie di Terze Parti
Nonostante le capacità native di Dart per la manipolazione delle date siano potenti, potresti trovarti nella necessità di operazioni più specifiche, come analizzare o formattare le date più facilmente, o eseguire calcoli complessi. In tali casi, il pacchetto `time` può essere molto utile.

Prima, aggiungi `time` alle tue dipendenze `pubspec.yaml`:

```yaml
dependencies:
  time: ^2.0.0
```

Poi, puoi usarlo per eseguire calcoli simili con una maggiore leggibilità:

```dart
import 'package:time/time.dart';

void main() {
  DateTime oggi = DateTime.now();

  // Calcolare una data futura
  DateTime dataFutura = oggi + 10.days;
  print(dataFutura); // Formato dell'output: 2023-04-21 14:22:35.123456

  // Calcolare una data passata
  DateTime dataPassata = oggi - 15.days;
  print(dataPassata); // Formato dell'output: 2023-03-27 14:22:35.123456
}
```

Questi esempi illustrano manipolazioni basilari delle date in Dart, inclusi l'aggiunta e la sottrazione di tempo da o verso una data corrente, dimostrando quanto sia agevole gestire le date nelle applicazioni Dart.
