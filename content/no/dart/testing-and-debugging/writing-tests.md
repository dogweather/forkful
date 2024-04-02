---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:53.245946-07:00
description: "\xC5 skrive tester i Dart involverer oppretting av testtilfeller for\
  \ automatisk \xE5 verifisere at forskjellige deler av programmet ditt fungerer som\
  \ forventet.\u2026"
lastmod: '2024-03-13T22:44:40.491212-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i Dart involverer oppretting av testtilfeller for automatisk\
  \ \xE5 verifisere at forskjellige deler av programmet ditt fungerer som forventet.\u2026"
title: Skrive tester
weight: 36
---

## Hva & Hvorfor?

Å skrive tester i Dart involverer oppretting av testtilfeller for automatisk å verifisere at forskjellige deler av programmet ditt fungerer som forventet. Programmerere gjør dette for å sikre at koden deres er pålitelig og fri for feil, noe som letter oppdateringer og refaktorering samtidig som det forhindrer regresjoner.

## Hvordan:

I Dart er `test`-pakken vanligvis brukt for å skrive tester. Først, legg til `test`-pakken i din `pubspec.yaml`:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Deretter skriver du en test for en enkel funksjon. Anta at du har en funksjon som legger sammen to tall:

```dart
int add(int a, int b) {
  return a + b;
}
```

Videre, opprett en fil med navn `add_test.dart` i `test`-mappen og skriv testtilfellet ditt:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Anta at din `add`-funksjon er i lib/add.dart

void main() {
  test('legger til to tall', () {
    var forventet = 3;
    expect(add(1, 2), equals(forventet));
  });
}
```

For å kjøre testene, bruk Dart-kommandoen:

```bash
$ dart test
```

Eksempel på utdata kan se slik ut:

```
00:01 +1: Alle tester bestått!
```

### Bruke et tredjepartsbibliotek: Mockito for mocking

For å teste kode som har komplekse avhengigheter, kan du bruke Mockito for å opprette mock-objekter. Først, legg til Mockito i din `pubspec.yaml`:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Anta at du har en klasse `UserRepository` som henter brukerdata, og du vil teste en `UserService` som avhenger av `UserRepository` uten å treffe en ekte database:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Opprett en Mock-klasse med bruk av Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService Tester', () {
    test('Henter bruker vellykket', () {
      // Opprett mock-instans
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Setter opp mock-atferd
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Testbruker'));

      // Bekrefter at den mockede metoden blir kalt med forventede argumenter
      expect(userService.getUserName(1), 'Testbruker');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Å kjøre denne testen bekrefter at `UserService` samhandler korrekt med `UserRepository`, ved å bruke mocking for å simulere de virkelige interaksjonene på en kontrollert måte.
