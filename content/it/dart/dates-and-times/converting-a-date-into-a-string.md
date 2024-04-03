---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:50.217044-07:00
description: "Convertire una data in una stringa in Dart \xE8 un compito comune quando\
  \ si ha la necessit\xE0 di visualizzare informazioni di data e ora in un formato\u2026"
lastmod: '2024-03-13T22:44:43.143788-06:00'
model: gpt-4-0125-preview
summary: "Convertire una data in una stringa in Dart \xE8 un compito comune quando\
  \ si ha la necessit\xE0 di visualizzare informazioni di data e ora in un formato\
  \ leggibile dall'uomo, o quando si intende serializzare i dati per la memorizzazione\
  \ o la trasmissione."
title: Convertire una data in una stringa
weight: 28
---

## Cosa e perché?

Convertire una data in una stringa in Dart è un compito comune quando si ha la necessità di visualizzare informazioni di data e ora in un formato leggibile dall'uomo, o quando si intende serializzare i dati per la memorizzazione o la trasmissione. Questo processo consente una facile rappresentazione e manipolazione dei valori di data e ora in un formato che è sia comprensibile che personalizzabile a seconda del caso d'uso.

## Come fare:

Dart fornisce la classe `DateTime` per gestire date e orari, e il pacchetto `intl` per la formattazione. Prima, assicurati di avere il pacchetto `intl` aggiungendo `intl: ^0.17.0` (o l'ultima versione) al tuo file `pubspec.yaml`.

### Utilizzando la Libreria Core di Dart

```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // Output: 2023-4-12 (ad esempio, questo dipende dalla data corrente)
```

Questo esempio costruisce direttamente una stringa dalle proprietà di `DateTime`.

### Utilizzando il pacchetto `intl`

Prima, importa il pacchetto:

```dart
import 'package:intl/intl.dart';
```

Quindi, formatta la data:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // Output: 2023-04-12
```

Il pacchetto `intl` consente una formattazione molto più complessa con facilità, inclusi i formati specifici per la località:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // Output: 12 aprile 2023
```

Questi esempi mostrano modi semplici ma potenti per convertire e formattare le date in stringhe in Dart, sia utilizzando le funzionalità core di Dart sia utilizzando il pacchetto `intl` per opzioni di formattazione più avanzate.
