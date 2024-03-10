---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:41.612256-07:00
description: "Tablice asocjacyjne w Dart, znane r\xF3wnie\u017C jako Mapy, to struktury\
  \ danych, kt\xF3re przechowuj\u0105 dane w parach klucz-warto\u015B\u0107. Umo\u017C\
  liwiaj\u0105 programistom dost\u0119p\u2026"
lastmod: '2024-03-09T21:05:59.818719-07:00'
model: gpt-4-0125-preview
summary: "Tablice asocjacyjne w Dart, znane r\xF3wnie\u017C jako Mapy, to struktury\
  \ danych, kt\xF3re przechowuj\u0105 dane w parach klucz-warto\u015B\u0107. Umo\u017C\
  liwiaj\u0105 programistom dost\u0119p\u2026"
title: Korzystanie z tablic asocjacyjnych
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne w Dart, znane również jako Mapy, to struktury danych, które przechowują dane w parach klucz-wartość. Umożliwiają programistom dostęp do elementów nie przez indeksy, ale klucze, czyniąc odzyskiwanie danych intuicyjnym i efektywnym, szczególnie przy pracy ze strukturalnymi danymi, gdzie każdy element ma unikalny identyfikator.

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
