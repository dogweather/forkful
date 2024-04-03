---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:14.431243-07:00
description: "Come: La libreria core di Dart semplifica l'analisi delle date attraverso\
  \ la classe `DateTime`. Per casi semplici in cui si conosce il formato della\u2026"
lastmod: '2024-03-13T22:44:43.141374-06:00'
model: gpt-4-0125-preview
summary: La libreria core di Dart semplifica l'analisi delle date attraverso la classe
  `DateTime`.
title: Analisi di una data da una stringa
weight: 30
---

## Come:
La libreria core di Dart semplifica l'analisi delle date attraverso la classe `DateTime`. Per casi semplici in cui si conosce il formato della stringa di data, è possibile utilizzare il metodo `DateTime.parse()`. Tuttavia, per scenari più complessi o quando si ha a che fare con formati multipli, il pacchetto `intl`, specificamente la classe `DateFormat`, diventa inestimabile.

### Utilizzando la Libreria Core di Dart:
```dart
void main() {
  // Utilizzando DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Utilizzando il pacchetto `intl`:
Prima, aggiungi il pacchetto `intl` al tuo file `pubspec.yaml`:
```yaml
dependencies:
  intl: ^0.17.0
```
Poi, importa il pacchetto e utilizza `DateFormat` per l'analisi:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
Il pacchetto `intl` offre opzioni robuste per l'analisi delle date, consentendo la gestione di vari formati internazionali di date in modo fluido.
