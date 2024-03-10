---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:01.770297-07:00
description: "Generowanie losowych liczb w Dart polega na tworzeniu warto\u015Bci\
  \ numerycznych, kt\xF3re s\u0105 nieprzewidywalne i r\xF3\u017Cni\u0105 si\u0119\
  \ przy ka\u017Cdym wykonaniu. Programi\u015Bci\u2026"
lastmod: '2024-03-09T21:05:59.821819-07:00'
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w Dart polega na tworzeniu warto\u015Bci numerycznych,\
  \ kt\xF3re s\u0105 nieprzewidywalne i r\xF3\u017Cni\u0105 si\u0119 przy ka\u017C\
  dym wykonaniu. Programi\u015Bci\u2026"
title: Generowanie liczb losowych
---

{{< edit_this_page >}}

## Co i dlaczego?
Generowanie losowych liczb w Dart polega na tworzeniu wartości numerycznych, które są nieprzewidywalne i różnią się przy każdym wykonaniu. Programiści wykorzystują tę funkcjonalność z różnych powodów, począwszy od symulowania scenariuszy świata rzeczywistego w środowiskach testowych, poprzez umożliwienie mechanik gier, aż po zapewnienie bezpieczeństwa poprzez losowość w operacjach kryptograficznych.

## Jak to zrobić:

Biblioteka podstawowa Darta zapewnia wsparcie dla generowania losowych liczb za pomocą klasy `Random`, znajdującej się w `dart:math`. Oto podstawowy przykład:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // Generuje losową liczbę całkowitą między 0 a 99
  double randomDouble = rand.nextDouble(); // Generuje losową liczbę zmiennoprzecinkową między 0.0 a 1.0
  print(randomNumber);
  print(randomDouble);
}
```

*Przykładowy wynik: (Będzie się różnić za każdym razem, gdy jest uruchomiony)*

```
23
0.6722390975465775
```

Dla przypadków użycia wymagających kryptograficznej losowości, Dart oferuje konstruktor `Random.secure`:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*Przykładowy wynik: (Będzie się różnić za każdym razem, gdy jest uruchomiony)*

```
45
```

Jeśli pracujesz nad projektami Fluttera lub potrzebujesz bardziej złożonej losowości, pakiet `faker` może okazać się przydatny do generowania szerokiego zakresu losowych danych, takich jak imiona, adresy i daty.

Aby użyć `faker`, najpierw dodaj go do pliku `pubspec.yaml`:

```yaml
dependencies:
  faker: ^2.0.0
```

Następnie zaimportuj go i użyj, jak pokazano:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // Generuje losowe imię
  print(faker.address.city()); // Generuje losową nazwę miasta
}
```

*Przykładowy wynik:*

```
Josie Runolfsdottir
East Lysanne
```
