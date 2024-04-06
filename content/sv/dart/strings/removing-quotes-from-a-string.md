---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:26.205853-07:00
description: "Hur man g\xF6r: Dart erbjuder enkla s\xE4tt att ta bort citattecken\
  \ fr\xE5n en str\xE4ng med hj\xE4lp av inbyggda str\xE4ngmetoder utan behov av tredjepartsbibliotek.\
  \ Om du\u2026"
lastmod: '2024-03-13T22:44:37.598250-06:00'
model: gpt-4-0125-preview
summary: "Dart erbjuder enkla s\xE4tt att ta bort citattecken fr\xE5n en str\xE4ng\
  \ med hj\xE4lp av inbyggda str\xE4ngmetoder utan behov av tredjepartsbibliotek."
title: "Ta bort citattecken fr\xE5n en str\xE4ng"
weight: 9
---

## Hur man gör:
Dart erbjuder enkla sätt att ta bort citattecken från en sträng med hjälp av inbyggda strängmetoder utan behov av tredjepartsbibliotek.

### Exempel 1: Använda `replaceFirst` och `replaceAll`
Om du hanterar strängar som börjar och slutar med citattecken kan du använda metoder som `replaceFirst` och `replaceAll` för att ta bort dem.

```dart
String quotedString = '"Hej, världen!"';
String singleQuotedString = '\'Dart-programmering\'';

// Ta bort dubbla citattecken
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Utdata: Hej, världen!

// Ta bort enkla citattecken
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Utdata: Dart-programmering
```

### Exempel 2: Använda `substring`
Den här metoden är användbar när du är säker på att citattecknen är precis i början och slutet av strängen.

```dart
String quotedString = '"Flutter-utveckling"';
// Kontrollera om den börjar och slutar med citattecken innan du tar bort dem för att undvika fel
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Utdata: Flutter-utveckling
```

### Exempel 3: Egen utökad metod
För större återanvändbarhet, särskilt om ditt projekt innebär frekvent borttagning av citat, överväg att skapa en egen utökning på `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"Det här är Dart"';
  String singleQuoted = '\'Det här är fantastiskt\'';
  print(doubleQuoted.unquote()); // Utdata: Det här är Dart
  print(singleQuoted.unquote()); // Utdata: Det här är fantastiskt
}
```

Dessa tillvägagångssätt bör hjälpa dig att effektivt ta bort citattecken från strängar i Dart, vilket förbättrar dina arbetsflöden för databehandling och förberedelse.
