---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:54.902524-07:00
description: "Come fare: Prima del refactoring, potresti avere un pezzo di codice\
  \ che mescola diversi livelli di astrazione o responsabilit\xE0, come calcolare\
  \ uno sconto\u2026"
lastmod: '2024-03-13T22:44:43.140267-06:00'
model: gpt-4-0125-preview
summary: ''
title: Refactoring
weight: 19
---

## Come fare:


### Esempio 1: Rinominare ed Estrazione di Metodi
Prima del refactoring, potresti avere un pezzo di codice che mescola diversi livelli di astrazione o responsabilità, come calcolare uno sconto e poi applicarlo:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Prezzo finale: $finalPrice");
}
```

**Output:**
```
Prezzo finale: 80.0
```

Dopo il refactoring, puoi estrarre il calcolo dello sconto in un proprio metodo e dargli un nome significativo:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calcolaPrezzoFinale(price, discount);
  print("Prezzo finale: $finalPrice");
}

double calcolaPrezzoFinale(double price, double discount) {
  return price - (price * discount);
}
```

**Output:**
```
Prezzo finale: 80.0
```

Estraendo il calcolo in un metodo, ora hai un'operazione chiaramente definita che può essere riutilizzata, testata indipendentemente e facilmente modificata.

### Esempio 2: Semplificazione delle Espressioni Condizionali
Prima del refactoring, le istruzioni condizionali potrebbero essere eccessivamente complesse o difficili da leggere:

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("Sconto: $discount");
}
```

**Output:**
```
Sconto: 0.05
```

Dopo il refactoring, considera l'uso di una mappa per una struttura più chiara e aggiornamenti o estensioni ai tipi di clienti e sconti più semplici:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Sconto: $discount");
}
```

**Output:**
```
Sconto: 0.05
```

Questo refactoring non solo rende il codice più conciso, ma anche incapsula la logica per determinare gli sconti in un modo più facile da comprendere e mantenere.

### Librerie di Terze Parti per il Refactoring
Quando si tratta di refactoring in Dart, specialmente all'interno delle app Flutter, la suite [Dart DevTools](https://dart.dev/tools/dart-devtools) è inestimabile. Include strumenti di performance, un ispettore di widget e un debugger a livello di codice sorgente. Sebbene non sia una libreria di terze parti, Dart DevTools è spesso usato insieme a librerie come `flutter_bloc` per gestire in modo pulito lo stato in modo che favorisca il refactoring per migliorare la modularità e la leggibilità. Purtroppo, a causa del campo di applicazione di questa voce, esempi di codice specifici che utilizzano librerie di terze parti non saranno forniti qui, ma agli sviluppatori è incoraggiato esplorare questi strumenti per migliorare il processo di refactoring nelle loro applicazioni Dart/Flutter.
