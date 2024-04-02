---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:59.549185-07:00
description: "Extraer subcadenas se trata de recuperar partes espec\xEDficas de una\
  \ cadena bas\xE1ndose en sus posiciones o patrones. Los programadores hacen esto\
  \ para tareas\u2026"
lastmod: '2024-03-13T22:44:58.736489-06:00'
model: gpt-4-0125-preview
summary: "Extraer subcadenas se trata de recuperar partes espec\xEDficas de una cadena\
  \ bas\xE1ndose en sus posiciones o patrones. Los programadores hacen esto para tareas\u2026"
title: Extrayendo subcadenas
weight: 6
---

## ¿Qué y por qué?
Extraer subcadenas se trata de recuperar partes específicas de una cadena basándose en sus posiciones o patrones. Los programadores hacen esto para tareas como analizar la entrada del usuario, manipulación de datos o extracción de información relevante de fuentes de texto más grandes.

## Cómo hacerlo:
En Dart, puedes usar varios métodos para extraer subcadenas, como `substring()`, `split()` y expresiones regulares. Cada método sirve para diferentes propósitos y ofrece flexibilidad en el manejo de cadenas.

### Usando `substring()`:
El método `substring()` es sencillo. Especificas el índice de inicio (y opcionalmente, el final) para cortar la cadena.

```dart
void main() {
  String ejemplo = "¡Hola, Mundo!";
  String resultado = ejemplo.substring(7, 12);
  print(resultado); // Salida: Mundo
}
```

### Usando `split()`:
Divide una cadena en una lista de subcadenas basadas en un patrón (como un espacio o coma), y luego accede a la subcadena por índice.

```dart
void main() {
  String ejemplo = "Dart es divertido";
  List<String> partes = ejemplo.split(' ');
  String resultado = partes[1]; // Acceso por índice
  print(resultado); // Salida: es
}
```

### Usando Expresiones Regulares:
Para patrones complejos, la clase `RegExp` de Dart es poderosa. Úsala para coincidir con patrones y extraer subcadenas.

```dart
void main() {
  String ejemplo = "Correo: ejemplo@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String correo = regExp.stringMatch(ejemplo)!;
  print(correo); // Salida: ejemplo@mail.com
}
```

### Bibliotecas de Terceros:
Aunque la biblioteca estándar de Dart es bastante capaz, podrías encontrar escenarios donde una biblioteca de terceros podría simplificar tu tarea. Una elección popular para la manipulación de cadenas y coincidencia de patrones no se recomienda específicamente aquí, ya que las capacidades integradas de Dart a menudo son suficientes. Sin embargo, siempre verifica en [pub.dev](https://pub.dev) para cualquier biblioteca que pueda adaptarse mejor a tus necesidades específicas.
