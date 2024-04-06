---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:50.912313-07:00
description: "C\xF3mo hacerlo: Antes de la refactorizaci\xF3n, es posible que tengas\
  \ un fragmento de c\xF3digo que mezcla diferentes niveles de abstracci\xF3n o responsabilidades,\u2026"
lastmod: '2024-03-13T22:44:58.761892-06:00'
model: gpt-4-0125-preview
summary: ''
title: "Refactorizaci\xF3n"
weight: 19
---

## Cómo hacerlo:


### Ejemplo 1: Renombrar y Extraer Métodos
Antes de la refactorización, es posible que tengas un fragmento de código que mezcla diferentes niveles de abstracción o responsabilidades, como calcular un descuento y luego aplicarlo:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Precio final: $finalPrice");
}
```

**Salida:**
```
Precio final: 80.0
```

Después de la refactorización, puedes extraer el cálculo del descuento a su propio método y darle un nombre significativo:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calcularPrecioFinal(price, discount);
  print("Precio final: $finalPrice");
}

double calcularPrecioFinal(double price, double discount) {
  return price - (price * discount);
}
```

**Salida:**
```
Precio final: 80.0
```

Al extraer el cálculo a un método, ahora tienes una operación claramente definida que puede ser reutilizada, probada de manera independiente y modificada fácilmente.

### Ejemplo 2: Simplificar Expresiones Condicionales
Antes de la refactorización, las declaraciones condicionales podrían ser excesivamente complejas o difíciles de leer:

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

  print("Descuento: $discount");
}
```

**Salida:**
```
Descuento: 0.05
```

Después de la refactorización, considera usar un mapa para una estructura más clara y actualizaciones o extensiones más fáciles a tipos de clientes y descuentos:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Descuento: $discount");
}
```

**Salida:**
```
Descuento: 0.05
```

Esta refactorización no solo hace el código más conciso sino también encapsula la lógica para determinar descuentos de una manera que es más fácil de entender y mantener.

### Bibliotecas de Terceros para la Refactorización
Cuando se trata de refactorizar en Dart, especialmente dentro de aplicaciones Flutter, el conjunto de herramientas [Dart DevTools](https://dart.dev/tools/dart-devtools) es invaluable. Incluye herramientas de rendimiento, un inspector de widgets y un depurador a nivel de fuente. Aunque no es una biblioteca de terceros, Dart DevTools a menudo se usa junto con bibliotecas como `flutter_bloc` para manejar el estado de manera limpia de una forma que favorece la refactorización para mejorar la modularidad y legibilidad. Desafortunadamente, debido al alcance de esta entrada, ejemplos específicos de código usando bibliotecas de terceros no se proporcionarán aquí, pero se anima a los desarrolladores a explorar estas herramientas para mejorar el proceso de refactorización en sus aplicaciones Dart/Flutter.
