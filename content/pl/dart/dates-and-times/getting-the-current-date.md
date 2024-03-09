---
title:                "Pobieranie bieżącej daty"
date:                  2024-03-08T21:55:03.073034-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
