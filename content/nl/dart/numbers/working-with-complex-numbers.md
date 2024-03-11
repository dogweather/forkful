---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:44.375687-07:00
description: "Complexe getallen, bestaande uit een re\xEBel en een imaginair deel\
  \ (gewoonlijk aangeduid als a + bi), breiden het concept van de dimensieloze getallen\
  \ uit\u2026"
lastmod: '2024-03-11T00:14:24.314367-06:00'
model: gpt-4-0125-preview
summary: "Complexe getallen, bestaande uit een re\xEBel en een imaginair deel (gewoonlijk\
  \ aangeduid als a + bi), breiden het concept van de dimensieloze getallen uit\u2026"
title: Werken met complexe getallen
---

{{< edit_this_page >}}

## Wat & Waarom?

Complexe getallen, bestaande uit een reëel en een imaginair deel (gewoonlijk aangeduid als a + bi), breiden het concept van de dimensieloze getallen uit naar een tweedimensionale ruimte. Programmeurs werken met complexe getallen in vakgebieden zoals elektrotechniek, kwantumcomputing en vloeistofdynamica om verschijnselen te modelleren die niet kunnen worden weergegeven langs een enkele dimensie van alleen reële getallen.

## Hoe:

Dart zelf bevat geen ingebouwde bibliotheek voor complexe getallen, wat noodzaakt tot het implementeren van een aangepaste klasse voor complexe getallen of het gebruik van een bibliotheek van derden. Een populaire keuze voor wetenschappelijke berekeningstaken, die ondersteuning biedt voor complexe getallen, is `package:scidart`.

### Een Basis Complex Getal Klasse Implementeren

Voor eenvoudige bewerkingen kun je gemakkelijk je eigen klasse voor complexe getallen definiëren:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Optellen van twee complexe getallen
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Stringrepresentatie voor eenvoudige foutopsporing
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var nummer1 = Complex(3, 4);
  var nummer2 = Complex(1, 2);

  var som = nummer1 + nummer2;
  print(som);  // 4.0 + 6.0i
}
```

### SciDart Gebruiken voor Geavanceerde Bewerkingen

Voor meer complexe bewerkingen of wanneer prestaties cruciaal zijn, biedt de `package:scidart` uitgebreide ondersteuning voor complexe getallen onder andere wetenschappelijke rekenfunctionaliteiten. Voeg eerst SciDart toe aan je pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Zo voer je basisbewerkingen met complexe getallen uit met SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Creëren van complexe getallen
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Optellen
  var som = complexAdd(complexNum1, complexNum2);
  
  // Vermenigvuldigen
  var product = complexMultiply(complexNum1, complexNum2);

  print('Som: ${som.toString()}');  // Som: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // Product: Complex(real: -11.0, imaginary: 41.0)
}
```

Deze voorbeelden demonstreren de basismanipulatie en het gebruik van complexe getallen in Dart, zowel door aangepaste implementatie als via de SciDart-bibliotheek, waarmee de flexibiliteit en kracht van Dart voor wetenschappelijke berekeningstaken wordt benadrukt.
