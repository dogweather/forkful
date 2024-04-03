---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:07.073671-07:00
description: "Arredondar n\xFAmeros \xE9 o processo de ajustar um n\xFAmero para o\
  \ seu inteiro mais pr\xF3ximo ou para um n\xFAmero especificado de casas decimais.\
  \ Programadores\u2026"
lastmod: '2024-03-13T22:44:46.275952-06:00'
model: gpt-4-0125-preview
summary: "Arredondar n\xFAmeros \xE9 o processo de ajustar um n\xFAmero para o seu\
  \ inteiro mais pr\xF3ximo ou para um n\xFAmero especificado de casas decimais."
title: "Arredondamento de n\xFAmeros"
weight: 13
---

## Como fazer:
Dart fornece métodos nativos em seu tipo fundamental `num` para operações de arredondamento. Aqui, exploraremos métodos como `round()`, `floor()`, `ceil()`, e como arredondar para um número específico de casas decimais.

### Arredondando para o inteiro mais próximo:
```dart
var number = 3.56;
print(number.round()); // Saída: 4
```

### Arredondando para baixo:
```dart
print(number.floor()); // Saída: 3
```

### Arredondando para cima:
```dart
print(number.ceil()); // Saída: 4
```

### Arredondando para um número específico de casas decimais:
Para arredondar para um número específico de casas decimais, podemos usar o método `toStringAsFixed()`, que retorna uma string, ou usar uma combinação de `pow` de `dart:math` para um resultado numérico.

```dart
import 'dart:math';

var number = 3.56789;
String roundedString = number.toStringAsFixed(2); // Para fins de exibição
print(roundedString); // Saída: 3.57

double roundedNumber = double.parse(roundedString);
print(roundedNumber); // Saída: 3.57

// Alternativamente, para um resultado numérico:
double roundedToDecimal = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(roundedToDecimal); // Saída: 3.57
```

Embora a biblioteca central do Dart cubra a maioria das necessidades de arredondamento de forma eficaz, para operações matemáticas mais complexas ou requisitos precisos de arredondamento, bibliotecas como `decimal` podem ser úteis. A biblioteca `decimal` oferece uma maneira fácil de trabalhar com números decimais sem perder precisão, o que é especialmente útil para cálculos financeiros, mas para métodos simples de arredondamento como mostrado, a funcionalidade central do Dart geralmente é suficiente.
