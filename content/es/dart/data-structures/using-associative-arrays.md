---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:56.382687-07:00
description: "Los arreglos asociativos en Dart, com\xFAnmente conocidos como Mapas,\
  \ son estructuras de datos que almacenan datos en pares clave-valor. Permiten a\
  \ los\u2026"
lastmod: '2024-03-13T22:44:58.741223-06:00'
model: gpt-4-0125-preview
summary: "Los arreglos asociativos en Dart, com\xFAnmente conocidos como Mapas, son\
  \ estructuras de datos que almacenan datos en pares clave-valor. Permiten a los\u2026"
title: Usando arrays asociativos
weight: 15
---

## ¿Qué y Por Qué?

Los arreglos asociativos en Dart, comúnmente conocidos como Mapas, son estructuras de datos que almacenan datos en pares clave-valor. Permiten a los programadores acceder a elementos no a través de índices, sino de claves, haciendo que la recuperación de datos sea intuitiva y eficiente, especialmente cuando se trabaja con datos estructurados donde cada elemento tiene un identificador único.

## Cómo hacerlo:

Dart ofrece una sintaxis sencilla para crear y manipular Mapas. A continuación, se muestran ejemplos que demuestran operaciones básicas como la creación, adición de elementos y obtención de valores.

```dart
void main() {
  // Creando un mapa
  var coloresFrutas = {
    'manzana': 'rojo',
    'banana': 'amarillo',
    'uva': 'morado'
  };

  // Agregando un nuevo par clave-valor
  coloresFrutas['naranja'] = 'naranja';

  // Accediendo a un valor por su clave
  print(coloresFrutas['manzana']); // Salida: rojo

  // Actualizando un valor
  coloresFrutas['banana'] = 'verde';

  // Iterando sobre el Mapa
  coloresFrutas.forEach((fruta, color) {
    print('$fruta: $color');
  });
  // Salida de muestra:
  // manzana: rojo
  // banana: verde
  // uva: morado
  // naranja: naranja
}
```

Para estructuras de datos complejas o funcionalidad extendida, los programadores de Dart a menudo confían en bibliotecas adicionales. Una de estas bibliotecas es `collection` que proporciona tipos de colecciones avanzadas y utilidades. Aunque `collection` no modifica la manera básica en que se manejan los Mapas, los enriquece con funciones de utilidad y tipos de colecciones más sofisticados. Así es como podrías usarla para una tarea más específica, como ordenar un Mapa por sus valores:

Primero, asegúrate de que el paquete `collection` esté incluido en tu archivo `pubspec.yaml`:

```yaml
dependencies:
  collection: ^1.15.0
```

Luego, puedes usarlo de la siguiente manera:

```dart
import 'package:collection/collection.dart';

void main() {
  var coloresFrutas = {
    'manzana': 'rojo',
    'banana': 'amarillo',
    'uva': 'morado',
    'naranja': 'naranja'
  };

  // Ordenando el Mapa por sus valores (colores)
  var frutasOrdenadasPorColor = SplayTreeMap.from(
    coloresFrutas,
    (key1, key2) => coloresFrutas[key1]!.compareTo(coloresFrutas[key2]!)
  );

  print(frutasOrdenadasPorColor);
  // Salida:
  // {naranja: naranja, manzana: rojo, banana: amarillo, uva: morado}
}
```

Este ejemplo demuestra cómo ordenar las entradas de un Mapa basado en sus valores, mostrando cómo Dart y su vibrante ecosistema pueden manejar ágilmente arreglos asociativos para manipulaciones de datos más sofisticadas.
