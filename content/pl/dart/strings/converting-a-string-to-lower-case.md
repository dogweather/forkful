---
title:                "Konwersja ciągu znaków na małe litery"
date:                  2024-03-08T21:54:33.912976-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja ciągu znaków na małe litery to podstawowa operacja polegająca na przekształceniu wszystkich znaków w danym ciągu na ich odpowiedniki w małych literach. Programiści zazwyczaj wykonują tę operację, aby osiągnąć porównania niezależne od wielkości liter lub aby ujednolicić wprowadzane dane tekstowe dla dalszego przetwarzania, co sprawia, że aplikacje są bardziej przyjazne dla użytkownika, a dane bardziej spójne.

## Jak to zrobić:

W Dart można przekonwertować ciąg znaków na małe litery za pomocą metody `toLowerCase()`, którą dostarcza klasa `String`. Metoda ta zwraca nowy ciąg znaków ze wszystkimi wielkimi literami przekonwertowanymi na małe. Zobaczmy, jak to działa na prostym przykładzie:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Wynik: hello, world!
}
```

Dart nie wymaga zewnętrznych bibliotek do podstawowych zadań manipulowania ciągami, w tym konwertowania na małe litery, ponieważ klasa `String` w standardowej bibliotece jest dość obszerna. Jednakże, dla bardziej złożonych manipulacji obejmujących reguły specyficzne dla danej lokalizacji, możesz rozważyć pakiet `intl`, który zapewnia możliwości internacjonalizacji i lokalizacji, w tym konwersję wielkości liter w oparciu o lokalizację:

Aby użyć `intl`, dodaj go do swojego pliku `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Następnie możesz użyć metody `toLocaleLowerCase()` do konwersji ciągu znaków na małe litery w oparciu o konkretne lokalizacje:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Lokalizacja turecka
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Wynik: istanbul
  
  // Domyślna lokalizacja (en)
  print(originalString.toLowerCase()); // Wynik: i̇stanbul
}
```

W tym przykładzie zwróć uwagę, jak lokalizacja turecka poprawnie obsługuje 'i' bez kropki, co pokazuje znaczenie transformacji świadomych ustawień regionalnych w zinternacjonalizowanych aplikacjach.
