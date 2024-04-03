---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:12.055367-07:00
description: "C\xF3mo hacerlo: Dart hace que sea sencillo eliminar caracteres que\
  \ coinciden con un patr\xF3n predefinido utilizando expresiones regulares y el m\xE9\
  todo\u2026"
lastmod: '2024-03-13T22:44:58.730150-06:00'
model: gpt-4-0125-preview
summary: "Dart hace que sea sencillo eliminar caracteres que coinciden con un patr\xF3\
  n predefinido utilizando expresiones regulares y el m\xE9todo `replaceAll`."
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo hacerlo:
Dart hace que sea sencillo eliminar caracteres que coinciden con un patrón predefinido utilizando expresiones regulares y el método `replaceAll`. No se requieren librerías de terceros para el uso básico, haciendo que este enfoque sea muy accesible.

He aquí un ejemplo simple que demuestra cómo remover dígitos de una cadena de texto:

```dart
void main() {
  String stringWithDigits = 'Dart123 es divertido456';
  // Definir un patrón de expresión regular que coincida con todos los dígitos
  RegExp digitPattern = RegExp(r'\d');
  
  // Reemplazar todas las ocurrencias del patrón con una cadena vacía
  String result = stringWithDigits.replaceAll(digitPattern, '');
  
  print(result); // Salida: Dart es divertido
}
```

Supongamos que te enfrentas a un escenario más complejo, como la eliminación de caracteres especiales excepto espacios y puntuación. Así es cómo lo harías:

```dart
void main() {
  String messyString = 'Dart!@# es *&()divertido$%^';
  // Definir un patrón que coincida con todo excepto letras, números, espacios y puntuación
  RegExp specialCharPattern = RegExp(r'[^a-zA-Z0-9 \.,!?]');
  
  String cleanedString = messyString.replaceAll(specialCharPattern, '');
  
  print(cleanedString); // Salida: Dart! es divertido
}
```

Para tareas que requieren emparejamiento y reemplazo de patrones más avanzados, la documentación de la clase `RegExp` de Dart ofrece una inmersión profunda en expresiones más complejas y su uso. Sin embargo, los ejemplos anteriores cubren la mayoría de los casos de uso comunes para eliminar caracteres basados en patrones en la programación Dart.
