---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:02.723251-07:00
description: "Jak to zrobi\u0107: Przed refaktoryzacj\u0105 mo\u017Cesz mie\u0107\
  \ fragment kodu, kt\xF3ry miesza r\xF3\u017Cne poziomy abstrakcji lub odpowiedzialno\u015B\
  ci, jak obliczanie zni\u017Cki, a\u2026"
lastmod: '2024-03-13T22:44:35.100828-06:00'
model: gpt-4-0125-preview
summary: ''
title: Refaktoryzacja
weight: 19
---

## Jak to zrobić:


### Przykład 1: Zmiana nazw i wydzielanie metod
Przed refaktoryzacją możesz mieć fragment kodu, który miesza różne poziomy abstrakcji lub odpowiedzialności, jak obliczanie zniżki, a następnie jej stosowanie:

```dart
void main() {
  var cena = 100.0;
  var zniżka = 0.2;
  var cenaKońcowa = cena - (cena * zniżka);
  print("Cena końcowa: $cenaKońcowa");
}
```

**Wynik:**
```
Cena końcowa: 80.0
```

Po refaktoryzacji możesz wydzielić obliczenie zniżki do własnej metody i nadać jej znaczącą nazwę:

```dart
void main() {
  var cena = 100.0;
  var zniżka = 0.2;
  var cenaKońcowa = obliczCenaKońcowa(cena, zniżka);
  print("Cena końcowa: $cenaKońcowa");
}

double obliczCenaKońcowa(double cena, double zniżka) {
  return cena - (cena * zniżka);
}
```

**Wynik:**
```
Cena końcowa: 80.0
```

Dzięki wydzieleniu obliczenia do metody, masz teraz jasno określoną operację, która może być ponownie używana, testowana niezależnie i łatwo modyfikowana.

### Przykład 2: Uproszczenie wyrażeń warunkowych
Przed refaktoryzacją, instrukcje warunkowe mogą być zbyt skomplikowane lub trudne do odczytania:

```dart
void main() {
  var typKlienta = "regularny";
  double zniżka;
  
  if (typKlienta == "regularny") {
    zniżka = 0.05;
  } else if (typKlienta == "członek") {
    zniżka = 0.1;
  } else {
    zniżka = 0.0;
  }

  print("Zniżka: $zniżka");
}
```

**Wynik:**
```
Zniżka: 0.05
```

Po refaktoryzacji, rozważ użycie mapy dla bardziej przejrzystej struktury i łatwiejszych aktualizacji lub rozszerzeń typów klientów i zniżek:

```dart
void main() {
  var typKlienta = "regularny";
  var zniżki = {
    "regularny": 0.05,
    "członek": 0.1,
    "brak": 0.0,
  };

  var zniżka = zniżki[typKlienta] ?? 0.0;
  print("Zniżka: $zniżka");
}
```

**Wynik:**
```
Zniżka: 0.05
```

Ta refaktoryzacja nie tylko czyni kod bardziej zwięzłym, ale także kapsułkuje logikę ustalania zniżek w sposób, który jest łatwiejszy do zrozumienia i utrzymania.

### Biblioteki stron trzecich do refaktoryzacji
Jeśli chodzi o refaktoryzację w Dart, zwłaszcza w aplikacjach Flutter, zestaw narzędzi [Dart DevTools](https://dart.dev/tools/dart-devtools) jest nieoceniony. Zawiera narzędzia do monitorowania wydajności, inspektora widżetów i debugera na poziomie źródła. Mimo że Dart DevTools nie jest biblioteką stron trzecich, często używa się go wraz z bibliotekami takimi jak `flutter_bloc` do czystego zarządzania stanem w sposób sprzyjający refaktoryzacji dla lepszej modularności i czytelności. Niestety, ze względu na zakres tego wpisu, tutaj nie zostaną przedstawione konkretne przykłady kodu z użyciem bibliotek stron trzecich, ale deweloperzy są zachęcani do eksplorowania tych narzędzi w celu ulepszenia procesu refaktoryzacji w ich aplikacjach Dart/Flutter.
