---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:19.845633-07:00
description: "Jak: Dart u\u017Cywa klasy `RegExp` dla wyra\u017Ce\u0144 regularnych.\
  \ Oto podstawowy przyk\u0142ad dopasowania prostego wzorca w ci\u0105gu znak\xF3\
  w."
lastmod: '2024-03-13T22:44:35.079627-06:00'
model: gpt-4-0125-preview
summary: "Dart u\u017Cywa klasy `RegExp` dla wyra\u017Ce\u0144 regularnych."
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
weight: 11
---

## Jak:
Dart używa klasy `RegExp` dla wyrażeń regularnych. Oto podstawowy przykład dopasowania prostego wzorca w ciągu znaków:

```dart
void main() {
  var wzorzec = RegExp(r'\bDart\b');
  var tekst = 'Uczenie się programowania w Dart jest ekscytujące.';

  if (wzorzec.hasMatch(tekst)) {
    print('Znaleziono dopasowanie!');
  } else {
    print('Nie znaleziono dopasowania.');
  }
  // Wynik: Znaleziono dopasowanie!
}
```

Aby wydobyć dopasowania z ciągu znaków, możesz użyć metody `allMatches`. Ta metoda zwraca iterowalną kolekcję dopasowań:

```dart
void main() {
  var wzorzec = RegExp(r'\b\w+\b');
  var tekst = 'Dart jest niesamowity!';

  var dopasowania = wzorzec.allMatches(tekst);
  for (finalne dopasowanie in dopasowania) {
    print(dopasowanie.group(0)); // To wypisuje dopasowane podciągi.
  }
  // Wynik:
  // Dart
  // jest
  // niesamowity
}
```

Zastępowanie tekstu można osiągnąć za pomocą metod `replaceFirst` lub `replaceAll`:

```dart
void main() {
  var wzorzec = RegExp(r'\bDart\b');
  var tekst = 'Dart nie jest tylko dartem.';
  
  // Zastąp pierwsze wystąpienie
  var zmodyfikowanyTekst = tekst.replaceFirst(wzorzec, 'Flutter');
  print(zmodyfikowanyTekst); 
  // Wynik: Flutter nie jest tylko dartem.

  // Zastąp wszystkie wystąpienia
  zmodyfikowanyTekst = tekst.replaceAll(wzorzec, 'Flutter');
  print(zmodyfikowanyTekst);
  // Wynik: Flutter nie jest tylko flutterem.
}
```

Dzielenie ciągu za pomocą wzorca regex jest proste dzięki metodzie `split`:

```dart
void main() {
  var wzorzec = RegExp(r'\s+'); // Dopasowuje każdy biały znak
  var tekst = 'Dart jest fajny';

  var części = tekst.split(wzorzec);
  print(części); 
  // Wynik: [Dart, jest, fajny]
}
```

Dla skomplikowanych zadań parsowania lub walidacji nieobsługiwanych bezpośrednio przez `RegExp` w Dart, można rozważyć użycie bibliotek firm trzecich, ale standardowa biblioteka Dart często wystarcza do powszechnych zadań z regex, podkreślając jej użyteczność i wszechstronność w obsługiwaniu wyrażeń regularnych.
