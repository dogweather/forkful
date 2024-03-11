---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:22.331428-07:00
description: "Zaokr\u0105glanie liczb to proces dopasowywania liczby do jej najbli\u017C\
  szej liczby ca\u0142kowitej lub do okre\u015Blonej liczby miejsc dziesi\u0119tnych.\
  \ Programi\u015Bci cz\u0119sto\u2026"
lastmod: '2024-03-11T00:14:08.256183-06:00'
model: gpt-4-0125-preview
summary: "Zaokr\u0105glanie liczb to proces dopasowywania liczby do jej najbli\u017C\
  szej liczby ca\u0142kowitej lub do okre\u015Blonej liczby miejsc dziesi\u0119tnych.\
  \ Programi\u015Bci cz\u0119sto\u2026"
title: "Zaokr\u0105glanie liczb"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zaokrąglanie liczb to proces dopasowywania liczby do jej najbliższej liczby całkowitej lub do określonej liczby miejsc dziesiętnych. Programiści często zaokrąglają liczby, aby uprościć obliczenia, poprawić czytelność lub przygotować dane do wyświetlenia, zapewniając spójność i klarowność w wynikach numerycznych.

## Jak to zrobić:

Dart oferuje natywne metody w swoim podstawowym typie `num` dla operacji zaokrąglania. Tutaj przyjrzymy się metodom takim jak `round()`, `floor()`, `ceil()` oraz jak zaokrąglić do określonej liczby miejsc dziesiętnych.

### Zaokrąglanie do najbliższej liczby całkowitej:

```dart
var number = 3.56;
print(number.round()); // Wyświetla: 4
```

### Zaokrąglanie w dół:

```dart
print(number.floor()); // Wyświetla: 3
```

### Zaokrąglanie w górę:

```dart
print(number.ceil()); // Wyświetla: 4
```

### Zaokrąglanie do określonej liczby miejsc dziesiętnych:

Aby zaokrąglić do określonej liczby miejsc dziesiętnych, możemy użyć metody `toStringAsFixed()`, która zwraca string, lub użyć kombinacji `pow` z `dart:math` dla wyniku numerycznego.

```dart
import 'dart:math';

var number = 3.56789;
String zaokrąglonyString = number.toStringAsFixed(2); // Dla celów wyświetlania
print(zaokrąglonyString); // Wyświetla: 3.57

double zaokrąglonaLiczba = double.parse(zaokrąglonyString);
print(zaokrąglonaLiczba); // Wyświetla: 3.57

// Alternatywnie, dla wyniku numerycznego:
double zaokrągloneDoMiejscaDziesiętnego = (number * pow(10, 2)).round().toDouble() / pow(10, 2);
print(zaokrągloneDoMiejscaDziesiętnego); // Wyświetla: 3.57
```

Chociaż podstawowa biblioteka Darta skutecznie zaspokaja większość potrzeb zaokrąglania, dla bardziej złożonych operacji matematycznych lub precyzyjnych wymagań zaokrąglania, biblioteki takie jak `decimal` mogą być przydatne. Biblioteka `decimal` zapewnia łatwy sposób pracy z liczbami dziesiętnymi bez utraty precyzji, co jest szczególnie przydatne do obliczeń finansowych, ale dla prostych metod zaokrąglania, jak pokazano, funkcjonalność podstawowa Darta jest zazwyczaj wystarczająca.
