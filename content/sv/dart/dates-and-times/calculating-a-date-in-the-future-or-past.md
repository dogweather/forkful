---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:14.236975-07:00
description: "Att ber\xE4kna ett datum i framtiden eller f\xF6rflutet \xE4r en vanlig\
  \ uppgift f\xF6r programmerare som hanterar schemal\xE4ggning, p\xE5minnelser eller\
  \ n\xE5gon funktion som\u2026"
lastmod: '2024-03-13T22:44:37.626647-06:00'
model: gpt-4-0125-preview
summary: "Att ber\xE4kna ett datum i framtiden eller f\xF6rflutet \xE4r en vanlig\
  \ uppgift f\xF6r programmerare som hanterar schemal\xE4ggning, p\xE5minnelser eller\
  \ n\xE5gon funktion som beror p\xE5 datumber\xE4kningar."
title: "Ber\xE4kna ett datum i framtiden eller f\xF6rflutna"
weight: 26
---

## Hur man gör:
Dart erbjuder robust stöd för datummanipulation genom sin `DateTime`-klass. Så här kan du beräkna framtida eller förflutna datum med hjälp av native Dart, utan att behöva tredjepartsbibliotek.

### Beräkna ett framtida datum
För att beräkna ett datum i framtiden skapar du ett `DateTime`-objekt och använder `add`-metoden med den önskade varaktigheten.

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // Output: 2023-04-21 14:22:35.123456 (exempelutmatning, beror på aktuellt datum och tid)
```

### Beräkna ett förflutet datum
För att beräkna ett datum i det förflutna använder du `subtract`-metoden på ett `DateTime`-objekt med den nödvändiga varaktigheten.

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // Output: 2023-03-27 14:22:35.123456 (exempelutmatning, beror på aktuellt datum och tid)
```

### Använda tredjepartsbibliotek
Även om Darts inhemska kapaciteter för datummanipulation är kraftfulla, kan du finna dig själv i behov av mer specifika operationer, som att enklare tolka eller formatera datum, eller utföra komplexa beräkningar. I sådana fall kan `time`-paketet vara mycket användbart.

Lägg först till `time` i dina `pubspec.yaml`-beroenden:

```yaml
dependencies:
  time: ^2.0.0
```

Sedan kan du använda det för att utföra liknande beräkningar med förbättrad läsbarhet:

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // Beräkna ett framtida datum
  DateTime futureDate = today + 10.days;
  print(futureDate); // Utmatningsformat: 2023-04-21 14:22:35.123456

  // Beräkna ett förflutet datum
  DateTime pastDate = today - 15.days;
  print(pastDate); // Utmatningsformat: 2023-03-27 14:22:35.123456
}
```

Dessa exempel illustrerar grundläggande datummanipulationer i Dart, inklusive att lägga till och dra ifrån tid till eller från ett aktuellt datum, och visar hur enkelt datum kan hanteras i Dart-applikationer.
