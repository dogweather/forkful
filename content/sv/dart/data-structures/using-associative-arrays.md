---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:58.831561-07:00
description: "Associativa arrayer i Dart, som vanligtvis kallas Maps, \xE4r datastrukturer\
  \ som lagrar data i nyckel-v\xE4rde-par. De m\xF6jligg\xF6r f\xF6r programmerare\
  \ att n\xE5\u2026"
lastmod: '2024-03-11T00:14:10.936139-06:00'
model: gpt-4-0125-preview
summary: "Associativa arrayer i Dart, som vanligtvis kallas Maps, \xE4r datastrukturer\
  \ som lagrar data i nyckel-v\xE4rde-par. De m\xF6jligg\xF6r f\xF6r programmerare\
  \ att n\xE5\u2026"
title: "Anv\xE4ndning av associativa arrayer"
---

{{< edit_this_page >}}

## Vad & Varför?

Associativa arrayer i Dart, som vanligtvis kallas Maps, är datastrukturer som lagrar data i nyckel-värde-par. De möjliggör för programmerare att nå element inte genom index, utan nycklar, vilket gör datahämtning intuitiv och effektiv, särskilt när man arbetar med strukturerad data där varje element har en unik identifierare.

## Hur gör man:

Dart erbjuder en enkel syntax för att skapa och manipulera Maps. Nedan följer exempel som demonstrerar grundläggande operationer som att skapa, lägga till element och hämta värden.

```dart
void main() {
  // Skapa en karta
  var fruitColors = {
    'apple': 'röd',
    'banana': 'gul',
    'grape': 'lila'
  };

  // Lägga till ett nytt nyckel-värde-par
  fruitColors['orange'] = 'orange';

  // Åtkomst av ett värde med dess nyckel
  print(fruitColors['apple']); // Utdata: röd

  // Uppdatera ett värde
  fruitColors['banana'] = 'grön';

  // Iterera över kartan
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // Exempelutdata:
  // apple: röd
  // banana: grön
  // grape: lila
  // orange: orange
}
```

För komplexa datastrukturer eller utökad funktionalitet förlitar sig Dart-programmerare ofta på ytterligare bibliotek. Ett sådant bibliotek är `collection` som tillhandahåller avancerade samlingstyper och verktyg. Även om `collection` inte ändrar det grundläggande sättet Maps hanteras, berikar det dem med verktygsfunktioner och mer sofistikerade samlingstyper. Så här kan du använda det för en mer specifik uppgift, såsom att sortera en Map efter dess värden:

Först, se till att `collection`-paketet är inkluderat i din `pubspec.yaml`-fil:

```yaml
dependencies:
  collection: ^1.15.0
```

Sedan kan du använda det som följer:

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'röd',
    'banana': 'gul',
    'grape': 'lila',
    'orange': 'orange'
  };

  // Sortera kartan efter dess värden (färger)
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // Utdata:
  // {orange: orange, apple: röd, banana: gul, grape: lila}
}
```

Detta exempel demonstrerar sortering av en Maps poster baserat på deras värden, vilket visar hur Dart och dess livliga ekosystem smidigt kan hantera associativa arrayer för mer sofistikerad datamanipulering.
