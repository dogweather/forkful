---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:10.651321-07:00
description: "Hur man g\xF6r: I Dart anv\xE4nds ofta paketet `test` f\xF6r att skriva\
  \ tester. F\xF6rst, l\xE4gg till paketet `test` i din `pubspec.yaml`."
lastmod: '2024-03-13T22:44:37.615893-06:00'
model: gpt-4-0125-preview
summary: "I Dart anv\xE4nds ofta paketet `test` f\xF6r att skriva tester."
title: Skriva tester
weight: 36
---

## Hur man gör:
I Dart används ofta paketet `test` för att skriva tester. Först, lägg till paketet `test` i din `pubspec.yaml`:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Skriv sedan ett test för en enkel funktion. Antag att du har en funktion som adderar två nummer:

```dart
int add(int a, int b) {
  return a + b;
}
```

Därefter, skapa en fil med namnet `add_test.dart` i `test`-katalogen och skriv ditt testfall:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Anta att din `add` funktion finns i lib/add.dart

void main() {
  test('lägger till två nummer', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

För att köra testerna, använd Dart-kommandot:

```bash
$ dart test
```

Exempel på utdata kan se ut som följer:

```
00:01 +1: Alla tester passerade!
```

### Använda ett tredjepartsbibliotek: Mockito för att mocka
För att testa kod som har komplexa beroenden kan du använda Mockito för att skapa mockobjekt. Först, lägg till Mockito i din `pubspec.yaml`:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Antag att du har en klass `UserRepository` som hämtar användardata, och du vill testa en `UserService` som beror på `UserRepository` utan att slå mot en riktig databas:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Skapa en Mock-klass med Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService-tester', () {
    test('Hämtar användare framgångsrikt', () {
      // Skapa mock-instans
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Konfigurera mock-beteende
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Test Användare'));

      // Kontrollera att den mockade metoden anropas med förväntade argument
      expect(userService.getUserName(1), 'Test Användare');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Att köra detta test bekräftar att `UserService` interagerar korrekt med `UserRepository`, genom att använda mocking för att simulera de verkliga interaktionerna på ett kontrollerat sätt.
