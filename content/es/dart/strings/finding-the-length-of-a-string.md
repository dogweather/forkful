---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:22.990562-07:00
description: "C\xF3mo hacerlo: Dart hace que sea sencillo obtener la longitud de una\
  \ cadena usando la propiedad `length`. Aqu\xED hay un ejemplo b\xE1sico."
lastmod: '2024-03-13T22:44:58.738923-06:00'
model: gpt-4-0125-preview
summary: Dart hace que sea sencillo obtener la longitud de una cadena usando la propiedad
  `length`.
title: Encontrando la longitud de una cadena
weight: 7
---

## Cómo hacerlo:
Dart hace que sea sencillo obtener la longitud de una cadena usando la propiedad `length`. Aquí hay un ejemplo básico:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("La longitud de '\(myString)' es: \(myString.length)");
  // Salida: La longitud de 'Hello, Dart!' es: 12
}
```
Esta propiedad cuenta el número de unidades de código UTF-16 en la cadena, lo cual corresponde a la longitud de la cadena para la mayoría de los casos de uso comunes.

Para un procesamiento de texto más matizado, especialmente involucrando caracteres Unicode fuera del Plano Multilingüe Básico (BMP), considere usar el paquete `characters` para contar grupos de grafemas, lo cual representa más precisamente los caracteres percibidos por el usuario.

Primero, agregue `characters` a su `pubspec.yaml`:

```yaml
dependencies:
  characters: ^1.2.0
```

Luego, úselo de esta manera:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 familia";
  print("La longitud de '\(myEmojiString)' es: \(myEmojiString.characters.length)");
  // Salida: La longitud de '👨‍👩‍👧‍👦 familia' es: 8
}
```

En este ejemplo, `myEmojiString.characters.length` nos da la longitud en términos de grupos de grafemas Unicode, lo cual es una representación más precisa para cadenas que contienen caracteres complejos, como emojis o marcas de caracteres combinados.
