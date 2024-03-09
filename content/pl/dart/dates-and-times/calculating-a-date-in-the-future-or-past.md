---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-03-08T21:53:53.633017-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości to powszechne zadanie dla programistów zajmujących się planowaniem, przypomnieniami lub jakąkolwiek funkcją zależną od obliczeń daty. Zrozumienie, jak manipulować datami, jest kluczowe dla systemów backendowych, interfejsów użytkownika i analizy danych, szczególnie dla tych, którzy przechodzą na Dart i chcą efektywnie wdrażać logikę czasową.

## Jak to zrobić:
Dart zapewnia solidne wsparcie dla manipulacji datami za pomocą klasy `DateTime`. Oto jak możesz obliczyć przyszłe lub przeszłe daty przy użyciu natywnego Darta, bez potrzeby korzystania z bibliotek innych firm.

### Obliczanie przyszłej daty
Aby obliczyć datę w przyszłości, tworzysz obiekt `DateTime` i używasz metody `add` z pożądanym czasem trwania.

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // Wyjście: 2023-04-21 14:22:35.123456 (przykładowe wyjście, zależne od bieżącej daty i godziny)
```

### Obliczanie przeszłej daty
Aby obliczyć datę w przeszłości, używasz metody `subtract` na obiekcie `DateTime` z koniecznym czasem trwania.

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // Wyjście: 2023-03-27 14:22:35.123456 (przykładowe wyjście, zależne od bieżącej daty i godziny)
```

### Korzystanie z bibliotek stron trzecich
Chociaż natywne możliwości Darta do manipulacji datami są potężne, może się zdarzyć, że potrzebujesz bardziej szczegółowych operacji, takich jak łatwiejsze parsowanie lub formatowanie dat, lub wykonywanie złożonych obliczeń. W takich przypadkach pakiet `time` może być bardzo przydatny.

Najpierw dodaj `time` do zależności w swoim `pubspec.yaml`:

```yaml
dependencies:
  time: ^2.0.0
```

Następnie możesz go użyć do wykonania podobnych obliczeń z lepszą czytelnością:

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // Obliczanie przyszłej daty
  DateTime futureDate = today + 10.days;
  print(futureDate); // Format wyjścia: 2023-04-21 14:22:35.123456

  // Obliczanie przeszłej daty
  DateTime pastDate = today - 15.days;
  print(pastDate); // Format wyjścia: 2023-03-27 14:22:35.123456
}
```

Te przykłady ilustrują podstawowe manipulacje datami w Darcie, w tym dodawanie i odejmowanie czasu do lub od bieżącej daty, demonstrując, jak bez wysiłku daty mogą być zarządzane w aplikacjach Dart.
