---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:39.565548-07:00
description: "Hur man g\xF6r: I Dart kan du anv\xE4nda olika metoder f\xF6r att extrahera\
  \ delstr\xE4ngar, s\xE5som `substring()`, `split()` och regulj\xE4ra uttryck. Varje\
  \ metod tj\xE4nar\u2026"
lastmod: '2024-03-13T22:44:37.599640-06:00'
model: gpt-4-0125-preview
summary: "I Dart kan du anv\xE4nda olika metoder f\xF6r att extrahera delstr\xE4ngar,\
  \ s\xE5som `substring()`, `split()` och regulj\xE4ra uttryck."
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## Hur man gör:
I Dart kan du använda olika metoder för att extrahera delsträngar, såsom `substring()`, `split()` och reguljära uttryck. Varje metod tjänar olika syften och erbjuder flexibilitet i hanteringen av strängar.

### Använda `substring()`:
Metoden `substring()` är enkel. Du anger startindexet (och valfritt, slutindexet) för att skära strängen.

```dart
void main() {
  String exempel = "Hej, världen!";
  String resultat = exempel.substring(7, 12);
  print(resultat); // Utdata: värld
}
```

### Använda `split()`:
Dela en sträng i en lista av delsträngar baserat på ett mönster (som ett mellanslag eller kommatecken) och sedan komma åt delsträngen med index.

```dart
void main() {
  String exempel = "Dart är kul";
  List<String> delar = exempel.split(' ');
  String resultat = delar[1]; // Komma åt med index
  print(resultat); // Utdata: är
}
```

### Använda reguljära uttryck:
För komplexa mönster är Darts `RegExp` klass kraftfull. Använd den för att matcha mönster och extrahera delsträngar.

```dart
void main() {
  String exempel = "E-post: exempel@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String epost = regExp.stringMatch(exempel)!;
  print(epost); // Utdata: exempel@mail.com
}
```

### Tredjepartsbibliotek:
Även om Darts standardbibliotek är ganska kapabelt, kan du stöta på scenarier där ett tredjepartsbibliotek skulle kunna förenkla din uppgift. Ett populärt val för stränghantering och mönstermatchning rekommenderas inte specifikt här eftersom Darts inbyggda funktioner ofta räcker till. Dock, kolla alltid [pub.dev](https://pub.dev) för alla bibliotek som kan passa dina specifika behov bättre.
