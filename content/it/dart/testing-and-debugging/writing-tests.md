---
title:                "Scrivere i test"
date:                  2024-03-08T21:57:54.239863-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere test in Dart implica la creazione di casi di test per verificare automaticamente che diverse parti del tuo programma funzionino come previsto. I programmatori fanno ciò per assicurarsi che il loro codice sia affidabile e privo di difetti, facilitando aggiornamenti e rifattorizzazioni più semplici, evitando al contempo regressioni.

## Come fare:

In Dart, il pacchetto `test` è comunemente utilizzato per scrivere test. Prima, aggiungi il pacchetto `test` al tuo `pubspec.yaml`:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Quindi, scrivi un test per una funzione semplice. Supponiamo che tu abbia una funzione che somma due numeri:

```dart
int add(int a, int b) {
  return a + b;
}
```

Successivamente, crea un file denominato `add_test.dart` nella directory `test` e scrivi il caso di test:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Supponiamo che la tua funzione `add` sia in lib/add.dart

void main() {
  test('somma due numeri', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

Per eseguire i test, utilizza il comando Dart:

```bash
$ dart test
```

L'output del campione potrebbe assomigliare a:

```
00:01 +1: Tutti i test superati!
```

### Utilizzo di una libreria di terze parti: Mockito per il mocking

Per testare codice che ha dipendenze complesse, potresti utilizzare Mockito per creare oggetti mock. Prima, aggiungi Mockito al tuo `pubspec.yaml`:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Supponendo di avere una classe `UserRepository` che recupera i dati degli utenti, e vuoi testare un `UserService` che dipende da `UserRepository` senza colpire un vero database:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Crea una classe Mock utilizzando Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('Test su UserService', () {
    test('Recupera utente con successo', () {
      // Crea istanza mock
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Impostazione del comportamento mock
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Test User'));

      // Assicurati che il metodo mockato sia chiamato con gli argomenti attesi
      expect(userService.getUserName(1), 'Test User');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Eseguire questo test conferma che `UserService` interagisce correttamente con `UserRepository`, utilizzando il mocking per simulare le interazioni reali in modo controllato.
