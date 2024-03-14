---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:21.343706-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP w j\u0119zyku Dart to proces inicjowania\
  \ komunikacji z serwerem internetowym lub API z aplikacji Dart. Programi\u015Bci\
  \ robi\u0105 to, aby\u2026"
lastmod: '2024-03-13T22:44:35.087646-06:00'
model: gpt-4-0125-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP w j\u0119zyku Dart to proces inicjowania\
  \ komunikacji z serwerem internetowym lub API z aplikacji Dart. Programi\u015Bci\
  \ robi\u0105 to, aby\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP w języku Dart to proces inicjowania komunikacji z serwerem internetowym lub API z aplikacji Dart. Programiści robią to, aby pobierać dane z sieci, wysyłać formularze i wchodzić w interakcje z usługami RESTful, co czyni to podstawową operacją dla rozwoju aplikacji internetowych, po stronie serwera i aplikacji mobilnych w Dart.

## Jak to zrobić:

Dart zawiera pakiet `http`, który jest potężnym i wygodnym sposobem na pracę z zasobami HTTP. Najpierw należy go dołączyć do pliku pubspec.yaml:

```yaml
dependencies:
  http: ^0.13.3
```

Następnie zaimportuj go w swoim kodzie Dart, aby zacząć wysyłać żądania:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Treść odpowiedzi: ${response.body}');
  } else {
    print('Żądanie zakończone niepowodzeniem ze statusem: ${response.statusCode}.');
  }
}
```

Przykładowy wynik dla udanego żądania może wyglądać tak:

```
Treść odpowiedzi: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Dla bardziej złożonych żądań, takich jak żądania POST z ciałem JSON, zrobiłbyś to w następujący sposób:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('Status odpowiedzi: ${response.statusCode}');
    print('Treść odpowiedzi: ${response.body}');
  } else {
    print('Nie udało się utworzyć nowego wpisu. Status: ${response.statusCode}');
  }
}
```

Przykładowy wynik dla żądania post może wyglądać tak:

```
Status odpowiedzi: 201
Treść odpowiedzi: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Te przykłady prezentują podstawowe żądania HTTP GET i POST za pomocą pakietu `http` w Dart. Pakiet ten pokrywa większość potrzeb związanych z wysyłaniem żądań HTTP, w tym bardziej złożone scenariusze z nagłówkami i zawartością ciała.
