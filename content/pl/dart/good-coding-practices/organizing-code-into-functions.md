---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:39.007943-07:00
description: "Organizacja kodu w funkcje w j\u0119zyku Dart polega na definiowaniu\
  \ ponownie wykorzystywalnych blok\xF3w kodu, kt\xF3re wykonuj\u0105 okre\u015Blone\
  \ zadania, zwykle\u2026"
lastmod: '2024-03-13T22:44:35.097513-06:00'
model: gpt-4-0125-preview
summary: "Organizacja kodu w funkcje w j\u0119zyku Dart polega na definiowaniu ponownie\
  \ wykorzystywalnych blok\xF3w kodu, kt\xF3re wykonuj\u0105 okre\u015Blone zadania,\
  \ zwykle\u2026"
title: Organizowanie kodu w funkcje
weight: 18
---

## Co i dlaczego?
Organizacja kodu w funkcje w języku Dart polega na definiowaniu ponownie wykorzystywalnych bloków kodu, które wykonują określone zadania, zwykle odbierając dane wejściowe, przetwarzając dane i możliwie zwracając dane wyjściowe. Programiści robią to, aby poprawić czytelność kodu, zmniejszyć duplikację i ułatwić konserwację, co ostatecznie prowadzi do bardziej modułowych i łatwiejszych do zarządzania baz kodów.

## Jak to zrobić:
### Podstawowa funkcja
W języku Dart definiujesz funkcję używając słowa kluczowego `void`, jeśli nie zwraca wartości, lub określasz typ zwracanej wartości w przeciwnym wypadku. Oto prosta funkcja, która drukuje wiadomość powitalną:

```dart
void greet(String name) {
  print('Cześć, $name!');
}

void main() {
  greet('Alicja');  // Wynik: Cześć, Alicja!
}
```

### Zwracanie wartości
Funkcje mogą zwracać wartości. Poniższy przykład pobiera dwie liczby całkowite jako dane wejściowe i zwraca ich sumę:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // Wynik: 8
}
```

### Funkcje anonimowe
Dart obsługuje funkcje anonimowe (znane również jako wyrażenia lambda lub zamknięcia), które mogą być przydatne dla krótkich, dorywczych funkcjonalności. Oto jak użyć funkcji anonimowej z metodą `forEach` listy:

```dart
void main() {
  var fruits = ['jabłko', 'banan', 'wiśnia'];
  fruits.forEach((item) {
    print(item);
  });
  // Wynik:
  // jabłko
  // banan
  // wiśnia
}
```

### Składnia strzałkowa dla funkcji jednowyrażeniowych
Dla funkcji, które zawierają tylko jedno wyrażenie, Dart oferuje zwięzłą składnię za pomocą notacji "strzałki" (`=>`). Jest to szczególnie przydatne dla krótkich funkcji lub przekazywania funkcji jako argumentów:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // Wynik: 16
}
```

### Korzystanie z bibliotek firm trzecich
Do bardziej złożonych lub specjalistycznych funkcjonalności programiści Dart często polegają na bibliotekach firm trzecich. Rozważ bibliotekę `http` do wykonywania żądań HTTP. Najpierw dodaj `http` do pliku pubspec.yaml w sekcji zależności:

```
dependencies:
  http: ^0.13.3
```

Następnie możesz jej użyć do pobierania danych z sieci:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // Oczekiwany wynik: Dane JSON użytkownika. Rzeczywisty wynik będzie zależał od odpowiedzi API.
}
```

Pamiętaj, organizując swój kod Dart w funkcje, myśl o wielokrotnym wykorzystaniu, przejrzystości i zasadzie pojedynczej odpowiedzialności. To nie tylko czyni twój kod czystszym, ale także łatwiejszym do zrozumienia i konserwacji dla innych (i przyszłego ciebie).
