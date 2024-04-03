---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:32.354609-07:00
description: "Hoe te: In Dart kun je verschillende methoden gebruiken om substrings\
  \ te extraheren, zoals `substring()`, `split()`, en reguliere expressies. Elke methode\u2026"
lastmod: '2024-03-13T22:44:50.493228-06:00'
model: gpt-4-0125-preview
summary: In Dart kun je verschillende methoden gebruiken om substrings te extraheren,
  zoals `substring()`, `split()`, en reguliere expressies.
title: Substrings extraheren
weight: 6
---

## Hoe te:
In Dart kun je verschillende methoden gebruiken om substrings te extraheren, zoals `substring()`, `split()`, en reguliere expressies. Elke methode dient verschillende doeleinden en biedt flexibiliteit in het omgaan met strings.

### Gebruikmakend van `substring()`:
De `substring()` methode is eenvoudig. Je geeft de start (en optioneel, het einde) index op om de string te slicen.

```dart
void main() {
  String voorbeeld = "Hallo, Wereld!";
  String resultaat = voorbeeld.substring(7, 12);
  print(resultaat); // Output: Wereld
}
```

### Gebruikmakend van `split()`:
Split een string in een lijst van substrings op basis van een patroon (zoals een spatie of komma), en benader vervolgens de substring op index.

```dart
void main() {
  String voorbeeld = "Dart is leuk";
  List<String> delen = voorbeeld.split(' ');
  String resultaat = delen[1]; // Benaderen op index
  print(resultaat); // Output: is
}
```

### Gebruikmakend van Reguliere Expressies:
Voor complexe patronen is Dart's `RegExp` klasse krachtig. Gebruik het om patronen te matchen en substrings te extraheren.

```dart
void main() {
  String voorbeeld = "Email: voorbeeld@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(voorbeeld)!;
  print(email); // Output: voorbeeld@mail.com
}
```

### Bibliotheek van Derden:
Hoewel de standaardbibliotheek van Dart vrij capabel is, kun je scenario's tegenkomen waar een bibliotheek van derden je taak zou kunnen vereenvoudigen. Een populaire keuze voor stringmanipulatie en patroonmatching wordt hier niet specifiek aangeraden, aangezien de ingebouwde mogelijkheden van Dart vaak voldoende zijn. Controleer echter altijd [pub.dev](https://pub.dev) voor eventuele bibliotheken die beter kunnen aansluiten bij je specifieke behoeften.
