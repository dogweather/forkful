---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:16.345147-07:00
description: "Das Schreiben von Tests in Dart umfasst das Erstellen von Testf\xE4\
  llen, um automatisch zu \xFCberpr\xFCfen, ob verschiedene Teile Ihres Programms\
  \ wie erwartet\u2026"
lastmod: '2024-03-09T21:06:17.579060-07:00'
model: gpt-4-0125-preview
summary: "Das Schreiben von Tests in Dart umfasst das Erstellen von Testf\xE4llen,\
  \ um automatisch zu \xFCberpr\xFCfen, ob verschiedene Teile Ihres Programms wie\
  \ erwartet\u2026"
title: Tests schreiben
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben von Tests in Dart umfasst das Erstellen von Testfällen, um automatisch zu überprüfen, ob verschiedene Teile Ihres Programms wie erwartet funktionieren. Programmierer tun dies, um sicherzustellen, dass ihr Code zuverlässig ist und frei von Fehlern, was einfachere Updates und Refaktorisierungen erleichtert, während Regressionen verhindert werden.

## Wie:

In Dart wird üblicherweise das `test`-Paket zum Schreiben von Tests verwendet. Fügen Sie zunächst das `test`-Paket zu Ihrer `pubspec.yaml` hinzu:

```yaml
dev_dependencies:
  test: ^1.0.0
```

Schreiben Sie dann einen Test für eine einfache Funktion. Nehmen wir an, Sie haben eine Funktion, die zwei Zahlen addiert:

```dart
int addiere(int a, int b) {
  return a + b;
}
```

Erstellen Sie anschließend eine Datei mit dem Namen `add_test.dart` im Verzeichnis `test` und schreiben Sie Ihren Testfall:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // Nehmen Sie an, Ihre `addieren`-Funktion befindet sich in lib/add.dart

void main() {
  test('addiert zwei Zahlen', () {
    var erwartet = 3;
    expect(addiere(1, 2), equals(erwartet));
  });
}
```

Um die Tests auszuführen, verwenden Sie den Dart-Befehl:

```bash
$ dart test
```

Die Ausgabe könnte folgendermaßen aussehen:

```
00:01 +1: Alle Tests bestanden!
```

### Verwendung einer Drittanbieter-Bibliothek: Mockito für das Mocking

Für das Testen von Code mit komplexen Abhängigkeiten könnten Sie Mockito verwenden, um Mock-Objekte zu erstellen. Fügen Sie zuerst Mockito zu Ihrer `pubspec.yaml` hinzu:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

Angenommen, Sie haben eine Klasse `BenutzerRepository`, die Benutzerdaten abruft, und Sie möchten einen `BenutzerService` testen, der von `BenutzerRepository` abhängt, ohne eine echte Datenbank zu verwenden:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/benutzer_repository.dart';
import 'package:your_project/benutzer_service.dart';

// Erstellen eines Mock-Objekts mit Mockito
class MockBenutzerRepository extends Mock implements BenutzerRepository {}

void main() {
  group('BenutzerService Tests', () {
    test('Ruft Benutzerdaten erfolgreich ab', () {
      // Mock-Instanz erstellen
      final mockBenutzerRepository = MockBenutzerRepository();
      final benutzerService = BenutzerService(mockBenutzerRepository);

      // Mock-Verhalten einrichten
      when(mockBenutzerRepository.fetchUser(1)).thenReturn(Benutzer(id: 1, name: 'Test Benutzer'));

      // Überprüfung, dass die gemockte Methode mit den erwarteten Argumenten aufgerufen wird
      expect(benutzerService.getBenutzerName(1), 'Test Benutzer');
      verify(mockBenutzerRepository.fetchUser(1)).called(1);
    });
  });
}
```

Die Ausführung dieses Tests bestätigt, dass `BenutzerService` korrekt mit `BenutzerRepository` interagiert, indem Mocking verwendet wird, um die echten Interaktionen auf kontrollierte Weise zu simulieren.
