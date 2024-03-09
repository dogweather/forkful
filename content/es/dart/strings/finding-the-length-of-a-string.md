---
title:                "Encontrando la longitud de una cadena"
date:                  2024-03-08T21:54:22.990562-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Qué y por qué?
Encontrar la longitud de una cadena (String) en Dart se trata de determinar el número de unidades de código (esencialmente, el número de caracteres si se piensa de manera simplista) en una cadena dada. Los programadores hacen esto para manipular cadenas más precisamente, como validar la entrada, truncar el texto de visualización o procesar formatos de datos donde la longitud importa (por ejemplo, protocolos con mensajes prefijados por longitud).

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
