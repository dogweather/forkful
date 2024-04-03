---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:52.235545-07:00
description: "\xC9crire des tests en Dart consiste \xE0 cr\xE9er des cas de test pour\
  \ v\xE9rifier automatiquement que diff\xE9rentes parties de votre programme fonctionnent\
  \ comme\u2026"
lastmod: '2024-03-13T22:44:57.393954-06:00'
model: gpt-4-0125-preview
summary: "\xC9crire des tests en Dart consiste \xE0 cr\xE9er des cas de test pour\
  \ v\xE9rifier automatiquement que diff\xE9rentes parties de votre programme fonctionnent\
  \ comme pr\xE9vu."
title: "R\xE9daction de tests"
weight: 36
---

## Comment faire :
En Dart, le package `test` est couramment utilisé pour écrire des tests. Tout d'abord, ajoutez le package `test` à votre `pubspec.yaml` :

```yaml
dev_dependencies:
  test: ^1.0.0
```

Ensuite, écrivez un test pour une fonction simple. Supposons que vous avez une fonction qui ajoute deux nombres :

```dart
int add(int a, int b) {
  return a + b;
}
```

Puis, créez un fichier nommé `add_test.dart` dans le répertoire `test` et écrivez votre cas de test :

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Supposons que votre fonction `add` se trouve dans lib/add.dart

void main() {
  test('ajoute deux nombres', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

Pour exécuter les tests, utilisez la commande Dart :

```bash
$ dart test
```

Un exemple de sortie pourrait ressembler à :

```
00:01 +1: Tous les tests ont réussi !
```

### Utiliser une bibliothèque tierce : Mockito pour le mocking
Pour tester le code qui a des dépendances complexes, vous pourriez utiliser Mockito pour créer des objets simulés. D'abord, ajoutez Mockito à votre `pubspec.yaml` :

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Supposons que vous avez une classe `UserRepository` qui récupère les données des utilisateurs, et vous souhaitez tester un `UserService` qui dépend de `UserRepository` sans toucher une vraie base de données :

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:votre_projet/user_repository.dart';
import 'package:votre_projet/user_service.dart';

// Créer une classe Mock en utilisant Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('Tests de UserService', () {
    test('Récupère l’utilisateur avec succès', () {
      // Créer une instance mock
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // Configurer le comportement simulé
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Test User'));

      // Affirmer que la méthode simulée est appelée avec les arguments attendus
      expect(userService.getUserName(1), 'Test User');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

Exécuter ce test confirme que `UserService` interagit correctement avec `UserRepository`, en utilisant le mocking pour simuler les véritables interactions d'une manière contrôlée.
