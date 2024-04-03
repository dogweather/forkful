---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:04.276484-07:00
description: "Come fare: In Dart, l'interpolazione delle stringhe \xE8 semplice, utilizzando\
  \ il simbolo `$` per interpolare direttamente espressioni all'interno di\u2026"
lastmod: '2024-03-13T22:44:43.113333-06:00'
model: gpt-4-0125-preview
summary: "In Dart, l'interpolazione delle stringhe \xE8 semplice, utilizzando il simbolo\
  \ `$` per interpolare direttamente espressioni all'interno di letterali di stringa."
title: Interpolazione di una stringa
weight: 8
---

## Come fare:
In Dart, l'interpolazione delle stringhe è semplice, utilizzando il simbolo `$` per interpolare direttamente espressioni all'interno di letterali di stringa:

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // Semplice interpolazione di variabili
  print('Learning $name in $year!');
  // Output: Imparare Dart nel 2023!
  
  // Interpolazione di espressioni
  print('Tra due anni, sarà il ${year + 2}.');
  // Output: Tra due anni, sarà il 2025.
}
```

Nel caso in cui si abbiano espressioni più complesse o si voglia eseguire operazioni all'interno della stringa stessa, racchiudere l'espressione in `${}`. Dart non ha librerie di terze parti particolarmente popolari specificamente per l'interpolazione delle stringhe poiché è nativamente ben attrezzato per gestire scenari vari e complessi.
