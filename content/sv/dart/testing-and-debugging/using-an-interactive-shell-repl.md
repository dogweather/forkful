---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:53.398451-07:00
description: "En interaktiv skal (REPL - Read-Evaluate-Print Loop) f\xF6r Dart g\xF6\
  r det m\xF6jligt f\xF6r programmerare att dynamiskt skriva och exekvera Dart-kod\
  \ rad f\xF6r rad\u2026"
lastmod: '2024-03-13T22:44:37.613721-06:00'
model: gpt-4-0125-preview
summary: "En interaktiv skal (REPL - Read-Evaluate-Print Loop) f\xF6r Dart g\xF6r\
  \ det m\xF6jligt f\xF6r programmerare att dynamiskt skriva och exekvera Dart-kod\
  \ rad f\xF6r rad\u2026"
title: "Anv\xE4nda en interaktiv skal (REPL)"
---

{{< edit_this_page >}}

## Vad & Varför?

En interaktiv skal (REPL - Read-Evaluate-Print Loop) för Dart gör det möjligt för programmerare att dynamiskt skriva och exekvera Dart-kod rad för rad utan att behöva kompilera hela skript. Detta verktyg är ovärderligt för att lära sig Darts syntax, experimentera med kodsnuttar eller felsöka genom att erbjuda omedelbar feedback och underlätta iterativa tester.

## Hur:

Dart kommer inte med en inbyggd REPL. Dock kan du uppnå REPL-liknande funktionalitet genom att använda DartPad (online) eller genom att utnyttja verktyg från tredje part som `dart_repl`.

**Använda DartPad:**

DartPad (https://dartpad.dev) är en online Dart-redigerare som låter dig skriva och köra Dart-kod i din webbläsare. Även om det inte är en traditionell kommandorads-REPL, erbjuder den en liknande erfarenhet för snabb experiment.

Gå helt enkelt till webbplatsen, skriv din Dart-kod i den vänstra rutan och klicka på "Kör" för att se utmatningen på höger sida.

Exempel:
```dart
void main() {
  print('Hej, Dart!');
}
```
Utskrift:
```
Hej, Dart!
```

**Använda `dart_repl` (verktyg från tredje part):**

Först, installera `dart_repl` via pub globalt:

```shell
dart pub global activate dart_repl
```

Sedan kör du `dart_repl` från din terminal:

```shell
dart_repl
```

Nu kan du börja skriva Dart-uttalanden direkt i skalet. Till exempel:

```dart
>>> print('Hej, REPL!');
Hej, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Dessa metoder ger en snabb väg för att prova Dart-kod på flyget, vilket avsevärt underlättar inlärningskurvan och ökar produktiviteten.
