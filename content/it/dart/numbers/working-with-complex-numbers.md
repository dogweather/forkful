---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:04.247327-07:00
description: "I numeri complessi, consistendo di una parte reale e una immaginaria\
  \ (solitamente indicati come a + bi), estendono il concetto dei numeri adimensionali\
  \ a\u2026"
lastmod: '2024-03-13T22:44:43.122581-06:00'
model: gpt-4-0125-preview
summary: I numeri complessi, consistendo di una parte reale e una immaginaria (solitamente
  indicati come a + bi), estendono il concetto dei numeri adimensionali a uno spazio
  bidimensionale.
title: Lavorare con i numeri complessi
weight: 14
---

## Cosa & Perché?

I numeri complessi, consistendo di una parte reale e una immaginaria (solitamente indicati come a + bi), estendono il concetto dei numeri adimensionali a uno spazio bidimensionale. I programmatori lavorano con i numeri complessi in campi come l'ingegneria elettrica, il calcolo quantistico e la dinamica dei fluidi per modellare fenomeni che non possono essere rappresentati lungo una singola dimensione di numeri reali da soli.

## Come fare:

Dart di per sé non include una libreria integrata per i numeri complessi, necessitando quindi dell'implementazione di una propria classe di numeri complessi o dell'uso di una libreria di terze parti. Una scelta popolare per compiti di calcolo scientifico, che include il supporto per i numeri complessi, è `package:scidart`.

### Implementare una Classe di Base per i Numeri Complessi

Per operazioni semplici, è possibile definire facilmente la propria classe di numeri complessi:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Somma di due numeri complessi
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Rappresentazione come stringa per un facile debugging
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### Usare SciDart per Operazioni Avanzate

Per operazioni più complesse o quando la performance è critica, il `package:scidart` offre un supporto comprensivo per i numeri complessi tra le altre funzionalità di calcolo scientifico. Prima, aggiungi SciDart al tuo pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Ecco come eseguire operazioni di base con i numeri complessi usando SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Creazione di numeri complessi
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Somma
  var sum = complexAdd(complexNum1, complexNum2);
  
  // Moltiplicazione
  var product = complexMultiply(complexNum1, complexNum2);

  print('Somma: ${sum.toString()}');  // Somma: Complex(real: 7.0, imaginary: 10.0)
  print('Prodotto: ${product.toString()}');  // Prodotto: Complex(real: -11.0, imaginary: 41.0)
}
```

Questi esempi dimostrano la manipolazione di base e l'utilizzo dei numeri complessi in Dart, sia attraverso l'implementazione personalizzata che tramite la libreria SciDart, evidenziando la flessibilità e la potenza di Dart per compiti di calcolo scientifico.
