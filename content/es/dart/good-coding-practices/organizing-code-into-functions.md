---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:37.721427-07:00
description: "C\xF3mo hacerlo: #."
lastmod: '2024-03-13T22:44:58.758013-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Organizando el c\xF3digo en funciones"
weight: 18
---

## Cómo hacerlo:


### Función Básica
En Dart, defines una función usando la palabra clave `void` si no devuelve un valor, o especificas el tipo de valor que devuelve de lo contrario. Aquí hay una función simple que imprime un mensaje de saludo:

```dart
void greet(String name) {
  print('Hola, $name!');
}

void main() {
  greet('Alice');  // Salida: Hola, Alice!
}
```

### Devolviendo un Valor
Las funciones pueden devolver valores. El siguiente ejemplo toma dos enteros como entrada y devuelve su suma:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // Salida: 8
}
```

### Funciones Anónimas
Dart soporta funciones anónimas (también conocidas como expresiones lambda o closures), que pueden ser útiles para funcionalidades rápidas y al vuelo. Así es como se usa una función anónima con el método `forEach` de una lista:

```dart
void main() {
  var fruits = ['manzana', 'banana', 'cereza'];
  fruits.forEach((item) {
    print(item);
  });
  // Salida:
  // manzana
  // banana
  // cereza
}
```

### Sintaxis de Flecha para Funciones de Expresión Única
Para funciones que solo contienen una única expresión, Dart ofrece una sintaxis concisa usando la notación de "flecha" (`=>`). Esto es especialmente útil para funciones cortas o pasar funciones como argumentos:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // Salida: 16
}
```

### Usando Bibliotecas de Terceros
Para funcionalidades más complejas o especializadas, los programadores de Dart a menudo confían en bibliotecas de terceros. Considera la biblioteca `http` para hacer solicitudes HTTP. Primero, añade `http` a tu archivo pubspec.yaml bajo dependencias:

```
dependencies:
  http: ^0.13.3
```

Luego, puedes usarla para obtener datos de la web:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // Salida esperada: Datos JSON del usuario. La salida real dependerá de la respuesta de la API.
}
```

Recuerda, al organizar tu código Dart en funciones, piensa en la reutilización, claridad y el principio de responsabilidad única. Esto no solo hace que tu código sea más limpio, sino también más fácil para otros (y para ti en el futuro) de entender y mantener.
