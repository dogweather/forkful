---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:21.864828-07:00
description: "Att hantera fel i Dart handlar om att f\xF6rutse och hantera undantag\
  \ som uppst\xE5r under programk\xF6rning f\xF6r att \xF6ka tillf\xF6rlitligheten\
  \ och anv\xE4ndbarheten.\u2026"
lastmod: '2024-03-09T21:06:02.376189-07:00'
model: gpt-4-0125-preview
summary: "Att hantera fel i Dart handlar om att f\xF6rutse och hantera undantag som\
  \ uppst\xE5r under programk\xF6rning f\xF6r att \xF6ka tillf\xF6rlitligheten och\
  \ anv\xE4ndbarheten.\u2026"
title: Hantering av fel
---

{{< edit_this_page >}}

## Vad & Varför?
Att hantera fel i Dart handlar om att förutse och hantera undantag som uppstår under programkörning för att öka tillförlitligheten och användbarheten. Programmerare implementerar felhantering för att förhindra krascher och ge meningsfull återkoppling till användare, vilket säkerställer en mjukare, säkrare applikationsupplevelse.

## Hur gör man:
Dart stöder två typer av fel: *kompileringstidsfel* och *körtidsfel*. Kompileringstidsfel upptäcks av Dart-analysatorn innan koden körs, medan körtidsfel, eller undantag, inträffar under körning. Så här hanterar du undantag i Dart:

### Try-Catch
Använd `try-catch` för att fånga undantag och förhindra att de kraschar din applikation:

```dart
try {
  var result = 100 ~/ 0; // Försöker dela med noll, kastar ett undantag
} catch (e) {
  print('Fångade ett undantag: $e'); // Hanterar undantaget
}
```
Exempelutmatning: `Fångade ett undantag: IntegerDivisionByZeroException`

### Specifikt Undantag
För att hantera specifika undantag, nämn undantaget efter `catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('Kan inte dela med noll.'); // Hanterar specifikt delningsfel genom noll
}
```
Exempelutmatning: `Kan inte dela med noll.`

### Stack Trace
För att få en stackspårning för felsökning, använd en andra parameter i catch-blocket:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('Undantag: $e');
  print('Stackspårning: $s'); // Skriver ut stackspårning för felsökning
}
```

### Finally
Använd `finally` för att exekvera kod efter try/catch, oavsett om ett undantag kastades:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('Fångade ett undantag: $e');
} finally {
  print('Detta utförs alltid.'); // Städkod eller slutsteg
}
```
Exempelutmatning:
```
Fångade ett undantag: IntegerDivisionByZeroException
Detta utförs alltid.
```

### Tredjepartsbibliotek
Även om Darts kärnbibliotek är robust för felhantering, kan du också använda tredjepartspaket som `dartz` för funktionell programmering som introducerar begrepp som `Either` och `Option` som kan användas för felhantering. Här är ett exempel på hur du använder `dartz` för felhantering:

1. Lägg till `dartz` i din `pubspec.yaml`-fil under beroenden:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Använd `Either` för att hantera fel på ett smidigt sätt i din Dart-kod:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('Kan inte dela med noll.');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('Fel: $left'), 
    (right) => print('Resultat: $right')
  );
}
```
Exempelutmatning: `Fel: Kan inte dela med noll.`

Den `Left` delen representerar vanligtvis felet, och den `Right` delen representerar framgång. Detta mönster möjliggör felhantering på ett mer funktionellt sätt, vilket erbjuder klarhet och kontroll över felhantering.
