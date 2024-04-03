---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:30.680270-07:00
description: "Att hitta l\xE4ngden p\xE5 en str\xE4ng i Dart handlar om att best\xE4\
  mma antalet kodenheter (i grunden antalet tecken om man t\xE4nker p\xE5 det p\xE5\
  \ ett f\xF6renklat s\xE4tt) i\u2026"
lastmod: '2024-03-13T22:44:37.601990-06:00'
model: gpt-4-0125-preview
summary: "Att hitta l\xE4ngden p\xE5 en str\xE4ng i Dart handlar om att best\xE4mma\
  \ antalet kodenheter (i grunden antalet tecken om man t\xE4nker p\xE5 det p\xE5\
  \ ett f\xF6renklat s\xE4tt) i en given str\xE4ng."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Vad & Varför?
Att hitta längden på en sträng i Dart handlar om att bestämma antalet kodenheter (i grunden antalet tecken om man tänker på det på ett förenklat sätt) i en given sträng. Programmerare gör detta för att mer precist kunna manipulera strängar, såsom att validera inmatning, trunkera visningstext eller bearbeta dataformat där längden spelar roll (t.ex. protokoll med längdprefixade meddelanden).

## Hur man gör:
Dart gör det enkelt att få reda på en strängs längd genom att använda `length`-egenskapen. Här är ett grundläggande exempel:

```dart
void main() {
  String myString = "Hello, Dart!";
  print("Längden på '\(myString)' är: \(myString.length)");
  // Utdata: Längden på 'Hello, Dart!' är: 12
}
```
Denna egenskap räknar antalet UTF-16 kodenheter i strängen, vilket motsvarar strängens längd för de flesta vanliga användningsfall.

För mer nyanserad textbehandling, särskilt vid hantering av Unicode-tecken utanför Basic Multilingual Plane (BMP), överväg att använda `characters`-paketet för att räkna grafemkluster, vilket representerar tecken som uppfattas av användaren mer exakt.

Först, lägg till `characters` i ditt `pubspec.yaml`:

```yaml
dependencies:
  characters: ^1.2.0
```

Använd sedan det så här:

```dart
import 'package:characters/characters.dart';

void main() {
  String myEmojiString = "👨‍👩‍👧‍👦 familj";
  print("Längden på '\(myEmojiString)' är: \(myEmojiString.characters.length)");
  // Utdata: Längden på '👨‍👩‍👧‍👦 familj' är: 8
}
```

I detta exempel ger `myEmojiString.characters.length` oss längden i termer av Unicode grafemkluster, vilket är en mer korrekt representation för strängar som innehåller komplexa tecken, som emojis eller kombinerade teckenmärken.
