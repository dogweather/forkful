---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:41.612256-07:00
description: "Jak to zrobi\u0107: Dart oferuje prost\u0105 sk\u0142adni\u0119 do tworzenia\
  \ i manipulowania Mapami. Poni\u017Cej znajduj\u0105 si\u0119 przyk\u0142ady demonstruj\u0105\
  ce podstawowe operacje, takie\u2026"
lastmod: '2024-03-13T22:44:35.083265-06:00'
model: gpt-4-0125-preview
summary: "Dart oferuje prost\u0105 sk\u0142adni\u0119 do tworzenia i manipulowania\
  \ Mapami."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

## Jak to zrobić:
Dart oferuje prostą składnię do tworzenia i manipulowania Mapami. Poniżej znajdują się przykłady demonstrujące podstawowe operacje, takie jak tworzenie, dodawanie elementów i pobieranie wartości.

```dart
void main() {
  // Tworzenie mapy
  var koloryOwocow = {
    'jabłko': 'czerwone',
    'banan': 'żółty',
    'winogrono': 'fioletowe'
  };

  // Dodawanie nowej pary klucz-wartość
  koloryOwocow['pomarańcza'] = 'pomarańczowy';

  // Dostęp do wartości przez jej klucz
  print(koloryOwocow['jabłko']); // Wynik: czerwone

  // Aktualizacja wartości
  koloryOwocow['banan'] = 'zielony';

  // Iteracja po Mapie
  koloryOwocow.forEach((owoc, kolor) {
    print('$owoc: $kolor');
  });
  // Przykładowy wynik:
  // jabłko: czerwone
  // banan: zielony
  // winogrono: fioletowe
  // pomarańcza: pomarańczowy
}
```

Dla złożonych struktur danych lub rozszerzonej funkcjonalności, programiści Dart często polegają na dodatkowych bibliotekach. Jedną z takich bibliotek jest `collection`, która zapewnia zaawansowane typy kolekcji i narzędzia. Chociaż `collection` nie modyfikuje podstawowego sposobu obsługi Map, wzbogaca je o funkcje pomocnicze i bardziej wyrafinowane typy kolekcji. Oto jak można jej używać do bardziej specyficznych zadań, takich jak sortowanie Mapy według jej wartości:

Najpierw upewnij się, że pakiet `collection` jest zawarty w twoim pliku `pubspec.yaml`:

```yaml
dependencies:
  collection: ^1.15.0
```

Następnie możesz go używać w następujący sposób:

```dart
import 'package:collection/collection.dart';

void main() {
  var koloryOwocow = {
    'jabłko': 'czerwone',
    'banan': 'żółty',
    'winogrono': 'fioletowe',
    'pomarańcza': 'pomarańczowy'
  };

  // Sortowanie Mapy według jej wartości (kolorów)
  var posortowaneOwoceWedlugKoloru = SplayTreeMap.from(
    koloryOwocow,
    (klucz1, klucz2) => koloryOwocow[klucz1]!.compareTo(koloryOwocow[klucz2]!)
  );

  print(posortowaneOwoceWedlugKoloru);
  // Wynik:
  // {pomarańcza: pomarańczowy, jabłko: czerwone, banan: żółty, winogrono: fioletowe}
}
```

Ten przykład demonstruje sortowanie wpisów Mapy na podstawie ich wartości, pokazując, jak Dart i jego dynamiczny ekosystem mogą zręcznie radzić sobie z tablicami asocjacyjnymi do bardziej wyrafinowanej manipulacji danymi.
