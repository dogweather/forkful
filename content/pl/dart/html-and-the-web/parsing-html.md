---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:43.768241-07:00
description: "Przetwarzanie HTML w programowaniu polega na ekstrakcji danych z dokument\xF3\
  w HTML. Programi\u015Bci robi\u0105 to, aby wchodzi\u0107 w interakcj\u0119 lub\
  \ scrapowa\u0107 zawarto\u015B\u0107\u2026"
lastmod: '2024-03-13T22:44:35.088722-06:00'
model: gpt-4-0125-preview
summary: "Przetwarzanie HTML w programowaniu polega na ekstrakcji danych z dokument\xF3\
  w HTML. Programi\u015Bci robi\u0105 to, aby wchodzi\u0107 w interakcj\u0119 lub\
  \ scrapowa\u0107 zawarto\u015B\u0107\u2026"
title: "Analiza sk\u0142adniowa HTML"
---

{{< edit_this_page >}}

## Co i dlaczego?
Przetwarzanie HTML w programowaniu polega na ekstrakcji danych z dokumentów HTML. Programiści robią to, aby wchodzić w interakcję lub scrapować zawartość internetową w celu ekstrakcji informacji, testowania lub automatyzacji, nawet kiedy nie są dostępne oficjalne API.

## Jak to zrobić:
Dart nie zapewnia wbudowanego wsparcia dla przetwarzania HTML w swoich bibliotekach podstawowych. Jednakże możesz użyć pakietu zewnętrznego, takiego jak `html`, do przetwarzania i manipulowania dokumentami HTML.

Najpierw dodaj pakiet `html` do swojego pliku `pubspec.yaml`:

```yaml
dependencies:
  html: ^0.15.0
```

Następnie zaimportuj pakiet do swojego pliku Dart:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Oto podstawowy przykład przetwarzania ciągu znaków zawierającego HTML i ekstrakcji danych:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Witaj, Dart!</h1>
      <p>To jest akapit w przykładowym HTML</p>
    </body>
  </html>
  """;

  // Przetwarzanie ciągu HTML
  Document document = parse(htmlDocument);

  // Ekstrakcja danych
  String title = document.querySelector('h1')?.text ?? "Nie znaleziono tytułu";
  String paragraph = document.querySelector('p')?.text ?? "Nie znaleziono akapitu";

  print('Tytuł: $title');
  print('Akapit: $paragraph');
}
```

Wyjście:

```
Tytuł: Witaj, Dart!
Akapit: To jest akapit w przykładowym HTML
```

Aby wchodzić w interakcję z rzeczywistymi stronami internetowymi, możesz połączyć przetwarzanie `html` z żądaniami HTTP (używając pakietu `http` do pobierania zawartości internetowej). Oto szybki przykład:

Najpierw dodaj pakiet `http` razem z `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Następnie pobierz i przetwórz stronę HTML z internetu:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Pobieranie strony internetowej
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Zakładając, że strona zawiera tagi <h1>, które Cię interesują
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Nagłówki: $headlines');
  } else {
    print('Żądanie zakończone niepowodzeniem ze statusem: ${response.statusCode}.');
  }
}
```

Uwaga: Technika web scrapingu pokazana powyżej powinna być używana odpowiedzialnie i zgodnie z warunkami korzystania ze strony internetowej.
