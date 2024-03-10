---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:38.856579-07:00
description: "Att g\xF6ra f\xF6rsta bokstaven i ett ord eller en hel mening till versal\
  \ inneb\xE4r att man \xE4ndrar den f\xF6rsta bokstaven till stor bokstav, medan\
  \ resten av tecknen\u2026"
lastmod: '2024-03-09T21:06:02.352040-07:00'
model: gpt-4-0125-preview
summary: "Att g\xF6ra f\xF6rsta bokstaven i ett ord eller en hel mening till versal\
  \ inneb\xE4r att man \xE4ndrar den f\xF6rsta bokstaven till stor bokstav, medan\
  \ resten av tecknen\u2026"
title: "G\xF6r om en str\xE4ng till versaler"
---

{{< edit_this_page >}}

## Vad & Varför?

Att göra första bokstaven i ett ord eller en hel mening till versal innebär att man ändrar den första bokstaven till stor bokstav, medan resten av tecknen behålls som de är. Programmerare använder ofta denna teknik för att formatera användarinmatningar eller visa text för att säkerställa konsekvens eller följa grammatiska regler i användargränssnitt.

## Hur man gör:

### Använda Dart:s inbyggda metoder

Dart erbjuder enkla, raka metoder för strängmanipulation. För att göra ett ord eller en mening med stor begynnelsebokstav skulle du typiskt ta det första tecknet, omvandla det till versal och sedan sammanfoga det med resten av strängen. Så här kan du implementera det:

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var exempel = "hello world";
  print(capitalize(exempel)); // Utdata: Hello world
}
```

### Göra första bokstaven stor i varje ord

För att göra första bokstaven i varje ord i en sträng stor, skulle du kunna dela strängen i ord, göra varje ett stort, och sedan sätta ihop dem igen:

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var exempel = "hello dart enthusiasts";
  print(capitalizeWords(exempel)); // Utdata: Hello Dart Enthusiasts
}
```

### Använda tredjepartbibliotek

Även om Darts standardbibliotek täcker grundläggande behov, kan vissa uppgifter vara mer bekvämt utförda med hjälp av tredjeparts paket. Ett populärt val för utökade strängmanipuleringsmöjligheter, inklusive att göra början av ord stort, är paketet [`recase`](https://pub.dev/packages/recase). Efter att ha lagt till det i ditt projekts `pubspec.yaml`, kan du enkelt göra strängar stora bland andra funktionaliteter:

```dart
import 'package:recase/recase.dart';

void main() {
  var exempel = "hello world";
  var rc = ReCase(exempel);

  print(rc.titleCase); // Utdata: Hello World
}
```

Med hjälp av `recase`, kan du göra individuella ord, hela meningar eller till och med följa andra bokstäveringskonventioner utan att manuellt hantera strängtransformationerna.
