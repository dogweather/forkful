---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:06.117248-07:00
description: "Hur: Dart anv\xE4nder klassen `RegExp` f\xF6r regulj\xE4ra uttryck.\
  \ H\xE4r \xE4r ett grundl\xE4ggande exempel f\xF6r att matcha ett enkelt m\xF6nster\
  \ inom en str\xE4ng."
lastmod: '2024-03-13T22:44:37.600711-06:00'
model: gpt-4-0125-preview
summary: "Dart anv\xE4nder klassen `RegExp` f\xF6r regulj\xE4ra uttryck."
title: "Anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur:
Dart använder klassen `RegExp` för reguljära uttryck. Här är ett grundläggande exempel för att matcha ett enkelt mönster inom en sträng:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Att lära sig Dart-programmering är spännande.';

  if (pattern.hasMatch(text)) {
    print('Matchning hittad!');
  } else {
    print('Ingen matchning hittad.');
  }
  // Utskrift: Matchning hittad!
}
```

För att extrahera matchningar från en sträng kan du använda metoden `allMatches`. Denna metod returnerar en iterable av matchningar:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart är fantastiskt!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Detta skriver ut de matchade delsträngarna.
  }
  // Utskrift:
  // Dart
  // är
  // fantastiskt
}
```

Att ersätta text kan uppnås med hjälp av metoderna `replaceFirst` eller `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart är inte bara en dart.';
  
  // Ersätt första förekomsten
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Utskrift: Flutter är inte bara en dart.

  // Ersätt alla förekomster
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Utskrift: Flutter är inte bara en flutter.
}
```

Att dela upp en sträng med ett regex-mönster är enkelt med hjälp av metoden `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Matchar alla vita tecken
  var text = 'Dart är roligt';

  var delar = text.split(pattern);
  print(delar); 
  // Utskrift: [Dart, är, roligt]
}
```

För komplext parsande eller valideringar som inte stöds direkt av Darts `RegExp`, kan du överväga tredjepartsbibliotek, men Darts standardbibliotek är ofta tillräckligt för vanliga regex-uppgifter, vilket betonar dess användbarhet och mångsidighet i hanteringen av reguljära uttryck.
