---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:57.725909-07:00
description: "Het schrijven van tests in Dart omvat het cre\xEBren van testgevallen\
  \ om automatisch te verifi\xEBren dat verschillende delen van je programma werken\
  \ zoals\u2026"
lastmod: '2024-03-13T22:44:50.508666-06:00'
model: gpt-4-0125-preview
summary: "Het schrijven van tests in Dart omvat het cre\xEBren van testgevallen om\
  \ automatisch te verifi\xEBren dat verschillende delen van je programma werken zoals\u2026"
title: Tests Schrijven
weight: 36
---

## Wat & Waarom?

Het schrijven van tests in Dart omvat het creëren van testgevallen om automatisch te verifiëren dat verschillende delen van je programma werken zoals verwacht. Programmeurs doen dit om ervoor te zorgen dat hun code betrouwbaar is en vrij van defecten, wat gemakkelijker updates en refactoring mogelijk maakt en regressies voorkomt.

## Hoe:

In Dart wordt het `test` pakket vaak gebruikt voor het schrijven van tests. Voeg eerst het `test` pakket toe aan je `pubspec.yaml`:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Schrijf vervolgens een test voor een eenvoudige functie. Stel je voor dat je een functie hebt die twee nummers optelt:

```dart
int add(int a, int b) {
  return a + b;
}
```

Maak daarna een bestand genaamd `add_test.dart` in de `test` directory en schrijf je testgeval:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Stel je voor dat je `add` functie in lib/add.dart is

void main() {
  test('voegt twee nummers toe', () {
    var verwacht = 3;
    expect(add(1, 2), equals(verwacht));
  });
}
```

Om de tests te draaien, gebruik je het Dart-commando:

```bash
$ dart test
```

Voorbeelduitvoer zou kunnen lijken op:

```
00:01 +1: Alle tests geslaagd!
```

### Gebruik maken van een externe bibliotheek: Mockito voor mocking

Voor het testen van code die complexe afhankelijkheden heeft, kun je Mockito gebruiken om mock-objecten te creëren. Voeg eerst Mockito toe aan je `pubspec.yaml`:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Stel dat je een klasse `UserRepository` hebt die gebruikersgegevens ophaalt, en je wilt een `UserService` testen die afhankelijk is van `UserRepository` zonder een echte database te raken:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Maak een Mock-klasse met Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService Tests', () {
    test('Haalt gebruiker succesvol op', () {
      // Maak mockinstantie
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Mockgedrag instellen
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Testgebruiker'));

      // Controleren dat de gemockte methode wordt aangeroepen met de verwachte argumenten
      expect(userService.getUserName(1), 'Testgebruiker');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Het uitvoeren van deze test bevestigt dat `UserService` correct interageert met `UserRepository`, waarbij mocking wordt gebruikt om de echte interacties op een gecontroleerde manier te simuleren.
