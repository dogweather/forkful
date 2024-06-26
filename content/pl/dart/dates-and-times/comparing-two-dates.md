---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:51.355383-07:00
description: "Jak to zrobi\u0107: W Dart mo\u017Cesz por\xF3wna\u0107 daty, u\u017C\
  ywaj\u0105c klasy `DateTime`, kt\xF3ra oferuje metody takie jak `isBefore`, `isAfter`\
  \ i `isAtSameMomentAs` do\u2026"
lastmod: '2024-03-13T22:44:35.105425-06:00'
model: gpt-4-0125-preview
summary: "W Dart mo\u017Cesz por\xF3wna\u0107 daty, u\u017Cywaj\u0105c klasy `DateTime`,\
  \ kt\xF3ra oferuje metody takie jak `isBefore`, `isAfter` i `isAtSameMomentAs` do\
  \ bezpo\u015Bredniego por\xF3wnania."
title: "Por\xF3wnywanie dw\xF3ch dat"
weight: 27
---

## Jak to zrobić:
W Dart możesz porównać daty, używając klasy `DateTime`, która oferuje metody takie jak `isBefore`, `isAfter` i `isAtSameMomentAs` do bezpośredniego porównania. Dodatkowo, różnicę między datami można określić za pomocą metody `difference()`, dostarczając obiekt `Duration`, który szczegółowo opisuje przedział między dwoma punktami w czasie.

Oto podstawowy przykład ilustrujący te koncepcje:

```dart
void main() {
  DateTime poczatekWydarzenia = DateTime(2023, 5, 15);
  DateTime koniecWydarzenia = DateTime(2023, 5, 20);
  
  // Sprawdzanie, czy jedna data jest przed drugą
  if (poczatekWydarzenia.isBefore(koniecWydarzenia)) {
    print("Data rozpoczęcia wydarzenia jest przed datą zakończenia.");
  }

  // Sprawdzanie, czy dwie daty są takie same
  if (!poczatekWydarzenia.isAtSameMomentAs(koniecWydarzenia)) {
    print("Daty rozpoczęcia i zakończenia nie są takie same.");
  }
  
  // Obliczanie różnicy między dwiema datami
  Duration czasTrwaniaWydarzenia = koniecWydarzenia.difference(poczatekWydarzenia);
  print("Wydarzenie trwa przez ${czasTrwaniaWydarzenia.inDays} dni.");
}

/*
Wyjście:
Data rozpoczęcia wydarzenia jest przed datą zakończenia.
Daty rozpoczęcia i zakończenia nie są takie same.
Wydarzenie trwa przez 5 dni.
*/
```

Dla bardziej zaawansowanych manipulacji z datami, takich jak konwersje formatów, przydatna może być klasa `DateFormat` z pakietu `intl`. Poniżej znajduje się przykład pokazujący, jak jej używać do formatowania i porównywania dat:

Najpierw dołącz pakiet `intl` do swojego pliku `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Następnie użyj go w następujący sposób:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime dataWyjazdu = DateTime(2023, 5, 15);
  DateTime dataPowrotu = DateTime.parse('2023-05-20');

  // Formatowanie dat
  var formatowanie = DateFormat('yyyy-MM-dd');
  print("Wyjazd: ${formatowanie.format(dataWyjazdu)}");
  print("Powrót: ${formatowanie.format(dataPowrotu)}");

  // Porównanie za pomocą sformatowanych łańcuchów
  if (formatowanie.format(dataWyjazdu) == formatowanie.format(dataPowrotu)) {
    print("Daty wyjazdu i powrotu są takie same.");
  } else {
    print("Daty wyjazdu i powrotu są różne.");
  }
}

/*
Wyjście:
Wyjazd: 2023-05-15
Powrót: 2023-05-20
Daty wyjazdu i powrotu są różne.
*/
```

Ten przykład pokazuje, jak porównać dwa obiekty `DateTime` bezpośrednio oraz za pomocą sformatowanych łańcuchów znaków dla porównań, które muszą ignorować określone składniki, takie jak czas.
