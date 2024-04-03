---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:06.079939-07:00
description: "C\xF3mo: Dart utiliza la clase `RegExp` para expresiones regulares.\
  \ Aqu\xED hay un ejemplo b\xE1sico para coincidir con un patr\xF3n simple dentro\
  \ de una cadena."
lastmod: '2024-03-13T22:44:58.737633-06:00'
model: gpt-4-0125-preview
summary: Dart utiliza la clase `RegExp` para expresiones regulares.
title: Usando expresiones regulares
weight: 11
---

## Cómo:
Dart utiliza la clase `RegExp` para expresiones regulares. Aquí hay un ejemplo básico para coincidir con un patrón simple dentro de una cadena:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Aprender a programar en Dart es emocionante.';

  if (pattern.hasMatch(text)) {
    print('¡Coincidencia encontrada!');
  } else {
    print('No se encontró ninguna coincidencia.');
  }
  // Salida: ¡Coincidencia encontrada!
}
```

Para extraer coincidencias de una cadena, puedes usar el método `allMatches`. Este método devuelve un iterable de coincidencias:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = '¡Dart es increíble!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Esto imprime las subcadenas que coinciden.
  }
  // Salida:
  // Dart
  // es
  // increíble
}
```

Reemplazar texto se puede lograr usando los métodos `replaceFirst` o `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart no es solo un dardo.';
  
  // Reemplazar primera ocurrencia
  var textoModificado = text.replaceFirst(pattern, 'Flutter');
  print(textoModificado); 
  // Salida: Flutter no es solo un dardo.

  // Reemplazar todas las ocurrencias
  textoModificado = text.replaceAll(pattern, 'Flutter');
  print(textoModificado);
  // Salida: Flutter no es solo un flutter.
}
```

Dividir una cadena por un patrón regex es sencillo usando el método `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Coincide con cualquier carácter de espacio en blanco
  var text = 'Dart es divertido';

  var partes = text.split(pattern);
  print(partes); 
  // Salida: [Dart, es, divertido]
}
```

Para análisis o validaciones complejas que no son soportadas directamente por `RegExp` de Dart, podrías considerar librerías de terceros, pero la biblioteca estándar de Dart es a menudo suficiente para tareas comunes de regex, enfatizando su utilidad y versatilidad en el manejo de expresiones regulares.
