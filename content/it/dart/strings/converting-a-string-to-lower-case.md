---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:08.240473-07:00
description: "Convertire una stringa in minuscolo \xE8 un'operazione fondamentale\
  \ che consiste nel trasformare tutti i caratteri di una stringa data nei loro equivalenti\u2026"
lastmod: '2024-03-13T22:44:43.114759-06:00'
model: gpt-4-0125-preview
summary: "Convertire una stringa in minuscolo \xE8 un'operazione fondamentale che\
  \ consiste nel trasformare tutti i caratteri di una stringa data nei loro equivalenti\
  \ minuscoli."
title: Convertire una stringa in minuscolo
weight: 4
---

## Cosa & Perché?

Convertire una stringa in minuscolo è un'operazione fondamentale che consiste nel trasformare tutti i caratteri di una stringa data nei loro equivalenti minuscoli. I programmatori eseguono tipicamente questa operazione per realizzare confronti che non tengano conto del maiuscolo/minuscolo o per standardizzare l'input di testo per ulteriori elaborazioni, rendendo le applicazioni più user-friendly e i dati più coerenti.

## Come fare:

In Dart, puoi convertire una stringa in minuscolo utilizzando il metodo `toLowerCase()` fornito dalla classe `String`. Questo metodo restituisce una nuova stringa con tutti i caratteri maiuscoli convertiti in minuscoli. Vediamo come funziona con un semplice esempio:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Output: hello, world!
}
```

Dart non richiede librerie esterne per le operazioni di manipolazione delle stringhe di base, inclusa la conversione in minuscolo, poiché la classe `String` della libreria standard è piuttosto completa. Tuttavia, per manipolazioni più complesse che coinvolgono regole specifiche per la località, potresti considerare il pacchetto `intl`, che fornisce strutture di internazionalizzazione e localizzazione, inclusa la conversione del caso in base alla località:

Per utilizzare `intl`, aggiungilo al tuo file `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Successivamente, puoi utilizzare il metodo `toLocaleLowerCase()` per convertire una stringa in minuscolo basandoti su specifiche località:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Località turca
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Output: istanbul
  
  // Località predefinita (en)
  print(originalString.toLowerCase()); // Output: i̇stanbul
}
```

In questo esempio, nota come la località turca gestisce correttamente la 'i' senza punto, evidenziando l'importanza delle trasformazioni consapevoli della località nelle applicazioni internazionalizzate.
