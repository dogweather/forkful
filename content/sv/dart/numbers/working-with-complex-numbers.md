---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:11.378932-07:00
description: "Komplexa tal, som best\xE5r av en reell och en imagin\xE4r del (vanligtvis\
  \ betecknad som a + bi), utvidgar begreppet odimensionella tal till ett\u2026"
lastmod: '2024-03-11T00:14:10.937229-06:00'
model: gpt-4-0125-preview
summary: "Komplexa tal, som best\xE5r av en reell och en imagin\xE4r del (vanligtvis\
  \ betecknad som a + bi), utvidgar begreppet odimensionella tal till ett\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad & Varför?

Komplexa tal, som består av en reell och en imaginär del (vanligtvis betecknad som a + bi), utvidgar begreppet odimensionella tal till ett tvådimensionellt rum. Programmerare arbetar med komplexa tal inom områden som elektroteknik, kvantdatorer och fluiddynamik för att modellera fenomen som inte kan representeras längs en enda dimension av reella tal ensamma.

## Hur:

Dart inkluderar inte i sig ett inbyggt bibliotek för komplexa tal, vilket kräver antingen implementeringen av en egen klass för komplexa tal eller användningen av ett tredjepartbibliotek. Ett populärt val för vetenskapliga beräkningsuppgifter, som inkluderar stöd för komplexa tal, är `package:scidart`.

### Implementera en grundläggande klass för komplexa tal

För enkla operationer kan du enkelt definiera din egen klass för komplexa tal:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // Addition av två komplexa tal
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // Strängrepresentation för enkel felsökning
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var nummer1 = Complex(3, 4);
  var nummer2 = Complex(1, 2);

  var summa = nummer1 + nummer2;
  print(summa);  // 4.0 + 6.0i
}
```

### Använda SciDart för avancerade operationer

För mer komplexa operationer eller när prestanda är kritisk, erbjuder `package:scidart` omfattande stöd för komplexa tal bland andra funktioner för vetenskaplig beräkning. Först, lägg till SciDart i din pubspec.yaml:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

Så här utför du grundläggande operationer med komplexa tal med hjälp av SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // Skapa komplexa tal
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // Addition
  var summa = complexAdd(complexNum1, complexNum2);
  
  // Multiplikation
  var produkt = complexMultiply(complexNum1, complexNum2);

  print('Summa: ${summa.toString()}');  // Summa: Complex(real: 7.0, imaginary: 10.0)
  print('Produkt: ${produkt.toString()}');  // Produkt: Complex(real: -11.0, imaginary: 41.0)
}
```

Dessa exempel demonstrerar grundläggande manipulering och användning av komplexa tal i Dart, både genom egen implementering och via SciDart-biblioteket, vilket framhäver Darts flexibilitet och kraft för vetenskapliga beräkningsuppgifter.
