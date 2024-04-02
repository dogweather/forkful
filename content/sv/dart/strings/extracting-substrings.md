---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:39.565548-07:00
description: "Att extrahera delstr\xE4ngar handlar om att h\xE4mta specifika delar\
  \ av en str\xE4ng baserat p\xE5 deras positioner eller m\xF6nster. Programmerare\
  \ g\xF6r detta f\xF6r\u2026"
lastmod: '2024-03-13T22:44:37.599640-06:00'
model: gpt-4-0125-preview
summary: "Att extrahera delstr\xE4ngar handlar om att h\xE4mta specifika delar av\
  \ en str\xE4ng baserat p\xE5 deras positioner eller m\xF6nster. Programmerare g\xF6\
  r detta f\xF6r\u2026"
title: "Extrahera delstr\xE4ngar"
weight: 6
---

## Vad & Varför?
Att extrahera delsträngar handlar om att hämta specifika delar av en sträng baserat på deras positioner eller mönster. Programmerare gör detta för uppgifter som att tolka användarinmatning, datahantering eller extrahera relevant information från större textkällor.

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
