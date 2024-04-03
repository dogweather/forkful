---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:03.073034-07:00
description: "Jak to zrobi\u0107: Podstawowa biblioteka Darta zapewnia prosty dost\u0119\
  p do aktualnej daty i czasu poprzez klas\u0119 `DateTime`. Oto podstawowy przyk\u0142\
  ad, jak pobra\u0107\u2026"
lastmod: '2024-03-13T22:44:35.103012-06:00'
model: gpt-4-0125-preview
summary: "Podstawowa biblioteka Darta zapewnia prosty dost\u0119p do aktualnej daty\
  \ i czasu poprzez klas\u0119 `DateTime`."
title: "Pobieranie bie\u017C\u0105cej daty"
weight: 29
---

## Jak to zrobić:
Podstawowa biblioteka Darta zapewnia prosty dostęp do aktualnej daty i czasu poprzez klasę `DateTime`. Oto podstawowy przykład, jak pobrać aktualną datę:

```dart
void main() {
  DateTime teraz = DateTime.now();
  print(teraz); // Przykładowy wynik: 2023-04-12 10:00:00.000
}
```

Jeśli potrzebujesz tylko części dotyczącej daty (rok, miesiąc, dzień), możesz sformatować obiekt `DateTime`:

```dart
void main() {
  DateTime teraz = DateTime.now();
  String sformatowanaData = "${teraz.year}-${teraz.month}-${teraz.day}";
  print(sformatowanaData); // Przykładowy wynik: 2023-04-12
}
```

Dart nie zawiera wbudowanej biblioteki do bardziej złożonego formatowania dat, ale możesz użyć do tego celu pakietu `intl`. Najpierw dodaj pakiet do swojego `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Następnie, możesz łatwo formatować daty:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime teraz = DateTime.now();
  String sformatowanaData = DateFormat('yyyy-MM-dd').format(teraz);
  print(sformatowanaData); // Przykładowy wynik: 2023-04-12
}
```

Aby odkryć bardziej zaawansowane opcje formatowania, zbadaj klasę `DateFormat` dostarczoną przez pakiet `intl`, która obsługuje szeroką gamę wzorców i ustawień regionalnych.
