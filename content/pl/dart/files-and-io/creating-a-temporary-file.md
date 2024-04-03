---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:08.682773-07:00
description: "Jak to zrobi\u0107: Biblioteka `dart:io` w j\u0119zyku Dart u\u0142\
  atwia tworzenie tymczasowych plik\xF3w za pomoc\u0105 klasy `Directory`. Oto prosty\
  \ spos\xF3b na stworzenie\u2026"
lastmod: '2024-03-13T22:44:35.113928-06:00'
model: gpt-4-0125-preview
summary: "Biblioteka `dart:io` w j\u0119zyku Dart u\u0142atwia tworzenie tymczasowych\
  \ plik\xF3w za pomoc\u0105 klasy `Directory`."
title: Tworzenie tymczasowego pliku
weight: 21
---

## Jak to zrobić:
Biblioteka `dart:io` w języku Dart ułatwia tworzenie tymczasowych plików za pomocą klasy `Directory`. Oto prosty sposób na stworzenie tymczasowego pliku i zapisanie do niego pewnych treści:

```dart
import 'dart:io';

Future<void> main() async {
  // Utwórz tymczasową ścieżkę (lokalizacja specyficzna dla systemu)
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // Utwórz tymczasowy plik wewnątrz tego katalogu
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Zapisz pewne treści do tymczasowego pliku
  await tempFile.writeAsString('To są pewne tymczasowe treści');

  print('Utworzono tymczasowy plik: ${tempFile.path}');

  // Przykładowy wynik: Utworzono tymczasowy plik: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### Użycie biblioteki innej firmy: `path_provider`
W przypadku aplikacji (szczególnie aplikacji mobilnych z Flutter), możesz chcieć tworzyć pliki tymczasowe w bardziej ujednolicony i zarządzalny sposób. Pakiet `path_provider` może pomóc Ci znaleźć odpowiedni katalog tymczasowy na różnych platformach (iOS, Android, itp.).

Najpierw dodaj `path_provider` do swojego `pubspec.yaml` w sekcji dependencies:

```yaml
dependencies:
  path_provider: ^2.0.9
```

A oto, jak możesz go użyć do utworzenia tymczasowego pliku:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // Pobierz katalog tymczasowy
  final Directory tempDir = await getTemporaryDirectory();

  // Utwórz tymczasowy plik wewnątrz tego katalogu
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // Zapisz pewne treści do tymczasowego pliku
  await tempFile.writeAsString('To są pewne tymczasowe treści z path_provider');

  print('Utworzono tymczasowy plik z path_provider: ${tempFile.path}');

  // Przykładowy wynik: Utworzono tymczasowy plik z path_provider: /tmp/my_temp_file.txt (ścieżka może się różnić w zależności od platformy)
}
```

Te fragmenty kodu ilustrują tworzenie i interakcję z tymczasowymi plikami w Dart, oferując prostą i praktyczną metodę zarządzania danymi do celów krótkoterminowych.
