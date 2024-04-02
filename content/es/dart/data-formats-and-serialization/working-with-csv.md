---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:06.660482-07:00
description: "Trabajar con archivos CSV (Valores Separados por Comas) implica analizar\
  \ y generar archivos de texto donde cada l\xEDnea mantiene valores separados por\u2026"
lastmod: '2024-03-13T22:44:58.778691-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con archivos CSV (Valores Separados por Comas) implica analizar\
  \ y generar archivos de texto donde cada l\xEDnea mantiene valores separados por\u2026"
title: Trabajando con CSV
weight: 37
---

## Qué y Por Qué?

Trabajar con archivos CSV (Valores Separados por Comas) implica analizar y generar archivos de texto donde cada línea mantiene valores separados por comas. Los programadores hacen esto para permitir el intercambio de datos entre diferentes aplicaciones o para facilitar el almacenamiento de datos en un formato ligero y legible por humanos.

## Cómo hacerlo:

Para manejar archivos CSV en Dart, generalmente se procesa el texto manualmente o se utilizan bibliotecas de terceros para simplificar la tarea. Aquí, veremos ambos enfoques.

### Analizando CSV Manualmente

Si tus necesidades son simples, podrías optar por analizar manualmente una cadena CSV. Esto se puede lograr usando las funciones de manipulación de cadenas del núcleo de Dart:

```dart
void main() {
  // Datos CSV de muestra
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Dividiendo los datos CSV en líneas
  List<String> lines = csvData.split('\n');
  
  // Analizando cada línea
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // Salida de los datos analizados
  print(data);
}

// Salida de muestra:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### Usando una Biblioteca de Terceros: `csv`

Para escenarios más complejos o para simplificar tu código, puedes usar una biblioteca de terceros popular como `csv`. Primero, agrégala a tu proyecto incluyendo `csv: ^5.0.0` (o la última versión) en tu archivo `pubspec.yaml` bajo `dependencies`. Luego úsala de la siguiente manera:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // Usa CsvToListConverter para analizar los datos CSV
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // El primer elemento de la lista contiene los encabezados
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // Eliminando la fila de encabezado antes de procesar más
  listData.removeAt(0);
  
  // Convertir a List<Map<String, dynamic>> para un formato más estructurado
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // Salida de los datos mapeados
  print(mappedData);
}

// Salida de muestra:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

Ambos métodos demuestran cómo trabajar con datos CSV: el primero manualmente, para fines de aprendizaje o cuando se trata de estructuras CSV muy simples; el segundo, aprovechando una biblioteca potente que simplifica el análisis y puede manejar varias complejidades del formato CSV.
