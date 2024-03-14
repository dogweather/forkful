---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:04.276484-07:00
description: "L'interpolazione delle stringhe \xE8 il processo di iniettare valori\
  \ di variabili direttamente nelle stringhe, spesso per creare messaggi significativi\
  \ senza\u2026"
lastmod: '2024-03-13T22:44:43.113333-06:00'
model: gpt-4-0125-preview
summary: "L'interpolazione delle stringhe \xE8 il processo di iniettare valori di\
  \ variabili direttamente nelle stringhe, spesso per creare messaggi significativi\
  \ senza\u2026"
title: Interpolazione di una stringa
---

{{< edit_this_page >}}

## Cos'è & Perché?

L'interpolazione delle stringhe è il processo di iniettare valori di variabili direttamente nelle stringhe, spesso per creare messaggi significativi senza l'uso di concatenazioni ingombranti. I programmatori lo fanno per avere un codice più pulito e leggibile e per prevenire errori che sono propensi a verificarsi nelle concatenazioni complesse di stringhe.

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
