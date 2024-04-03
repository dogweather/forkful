---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:50.937764-07:00
description: "C\xF3mo hacerlo: En Dart, el paquete `test` se utiliza com\xFAnmente\
  \ para escribir pruebas. Primero, a\xF1ade el paquete `test` a tu `pubspec.yaml`."
lastmod: '2024-03-13T22:44:58.755839-06:00'
model: gpt-4-0125-preview
summary: "En Dart, el paquete `test` se utiliza com\xFAnmente para escribir pruebas."
title: Escribiendo pruebas
weight: 36
---

## Cómo hacerlo:
En Dart, el paquete `test` se utiliza comúnmente para escribir pruebas. Primero, añade el paquete `test` a tu `pubspec.yaml`:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Luego, escribe una prueba para una función simple. Supongamos que tienes una función que suma dos números:

```dart
int add(int a, int b) {
  return a + b;
}
```

A continuación, crea un archivo llamado `add_test.dart` en el directorio `test` y escribe tu caso de prueba:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Suponemos que tu función `add` está en lib/add.dart

void main() {
  test('suma dos números', () {
    var esperado = 3;
    expect(add(1, 2), equals(esperado));
  });
}
```

Para ejecutar las pruebas, usa el comando Dart:

```bash
$ dart test
```

La salida de muestra podría parecerse a:

```
00:01 +1: ¡Todas las pruebas pasaron!
```

### Usando una biblioteca de terceros: Mockito para simular
Para probar código que tiene dependencias complejas, podrías usar Mockito para crear objetos simulados. Primero, añade Mockito a tu `pubspec.yaml`:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Suponiendo que tienes una clase `UserRepository` que recupera datos de usuario, y quieres probar un `UserService` que depende de `UserRepository` sin acceder a una base de datos real:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:tu_proyecto/user_repository.dart';
import 'package:tu_proyecto/user_service.dart';

// Crea una clase Mock usando Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('Pruebas de UserService', () {
    test('Recupera usuario exitosamente', () {
      // Crea instancia mock
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Configurando comportamiento mock
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Usuario de Prueba'));

      // Afirmar que el método simulado se llama con los argumentos esperados
      expect(userService.getUserName(1), 'Usuario de Prueba');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Ejecutar esta prueba confirma que `UserService` interactúa correctamente con `UserRepository`, usando la simulación para imitar las interacciones reales de manera controlada.
