---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:03.073034-07:00
description: "Pobieranie aktualnej daty w Darciu wi\u0105\u017Ce si\u0119 z zapytaniem\
  \ systemu o aktualn\u0105 dat\u0119 i czas. Ta funkcjonalno\u015B\u0107 jest powszechnie\
  \ u\u017Cywana w aplikacjach do\u2026"
lastmod: '2024-03-13T22:44:35.103012-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie aktualnej daty w Darciu wi\u0105\u017Ce si\u0119 z zapytaniem\
  \ systemu o aktualn\u0105 dat\u0119 i czas. Ta funkcjonalno\u015B\u0107 jest powszechnie\
  \ u\u017Cywana w aplikacjach do\u2026"
title: "Pobieranie bie\u017C\u0105cej daty"
weight: 29
---

## Co i dlaczego?
Pobieranie aktualnej daty w Darciu wiąże się z zapytaniem systemu o aktualną datę i czas. Ta funkcjonalność jest powszechnie używana w aplikacjach do funkcji takich jak znakowanie czasem zdarzeń, pokazywanie aktualnej daty użytkownikom czy obliczanie okresów czasu. Wiedza o tym, jak efektywnie pobierać i manipulować aktualną datą jest podstawą dla planowania, logowania i funkcji wrażliwych na czas.

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
