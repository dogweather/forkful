---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:34.739794-07:00
description: "C\xF3mo hacerlo: En Dart, trabajar con YAML generalmente involucra el\
  \ uso de una biblioteca de terceros, ya que el lenguaje no incluye capacidades de\u2026"
lastmod: '2024-03-13T22:44:58.776458-06:00'
model: gpt-4-0125-preview
summary: "En Dart, trabajar con YAML generalmente involucra el uso de una biblioteca\
  \ de terceros, ya que el lenguaje no incluye capacidades de an\xE1lisis de YAML\
  \ incorporadas."
title: Trabajando con YAML
weight: 41
---

## Cómo hacerlo:
En Dart, trabajar con YAML generalmente involucra el uso de una biblioteca de terceros, ya que el lenguaje no incluye capacidades de análisis de YAML incorporadas. Una elección popular es el paquete `yaml`. Para comenzar, necesitarás agregar este paquete a tu `pubspec.yaml`:

```yaml
dependencies:
  yaml: ^3.1.0
```

Recuerda ejecutar `pub get` para obtener el paquete.

### Leer YAML
Para leer un archivo YAML, primero, importa el paquete `yaml` y luego usa la función `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Salida: John Doe
}

```

Asumiendo que tu archivo `config.yaml` se ve así:

```yaml
name: John Doe
age: 30
```

### Escribir YAML
Aunque el paquete `yaml` es excelente para el análisis, no soporta la escritura en YAML. Para ello, podrías necesitar convertir tus datos a YAML manualmente o usar otro paquete si está disponible. O, de manera más directa, gestionar tus transformaciones de datos y sacarlos como cadenas que coincidan con la sintaxis de YAML:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // Salida: name: Jane Doe
                             //         age: 29
}
```

Este es un enfoque rudimentario y podría no ser adecuado para estructuras de datos complejas o características especiales de YAML. Para necesidades sofisticadas, podría ser necesario buscar o contribuir a un paquete de Dart más completo.
