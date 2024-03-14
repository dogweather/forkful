---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:19.845633-07:00
description: "Wyra\u017Cenia regularne (regex) w Dart oferuj\u0105 pot\u0119\u017C\
  ny spos\xF3b na wyszukiwanie i manipulowanie ci\u0105gami znak\xF3w, umo\u017Cliwiaj\u0105\
  c programistom wykonywanie\u2026"
lastmod: '2024-03-13T22:44:35.079627-06:00'
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne (regex) w Dart oferuj\u0105 pot\u0119\u017Cny spos\xF3\
  b na wyszukiwanie i manipulowanie ci\u0105gami znak\xF3w, umo\u017Cliwiaj\u0105\
  c programistom wykonywanie\u2026"
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne (regex) w Dart oferują potężny sposób na wyszukiwanie i manipulowanie ciągami znaków, umożliwiając programistom wykonywanie skomplikowanych zadań przetwarzania tekstu w sposób wydajny. Rozumiejąc regex, deweloperzy mogą szybko wykonywać walidacje tekstu, szukać wzorców oraz przekształcać tekst, co jest niezbędne do przetwarzania formularzy, analizy danych oraz ogólnej manipulacji ciągami znaków w nowoczesnych aplikacjach.

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
