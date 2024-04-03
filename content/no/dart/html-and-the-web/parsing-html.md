---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:40.330314-07:00
description: "Hvordan gj\xF8re det: Dart tilbyr ikke innebygd st\xF8tte for HTML-parsing\
  \ i sine kjernelibraryer. Du kan imidlertid bruke en tredjepartspakke som `html`\
  \ for \xE5\u2026"
lastmod: '2024-03-13T22:44:40.484755-06:00'
model: gpt-4-0125-preview
summary: "Dart tilbyr ikke innebygd st\xF8tte for HTML-parsing i sine kjernelibraryer."
title: Analysering av HTML
weight: 43
---

## Hvordan gjøre det:
Dart tilbyr ikke innebygd støtte for HTML-parsing i sine kjernelibraryer. Du kan imidlertid bruke en tredjepartspakke som `html` for å parse og manipulere HTML-dokumenter.

Først, legg til `html`-pakken i din `pubspec.yaml`-fil:

```yaml
dependencies:
  html: ^0.15.0
```

Deretter importerer du pakken inn i din Dart-fil:

```dart
import 'package:html/parser.dart' vis parse;
import 'package:html/dom.dart';
```

Her er et grunnleggende eksempel på parsing av en streng som inneholder HTML og ekstraksjon av data:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hei, Dart!</h1>
      <p>Dette er et avsnitt i et eksempel på HTML</p>
    </body>
  </html>
  """;

  // Parse HTML-strengen
  Document document = parse(htmlDocument);

  // Ekstrakter data
  String tittel = document.querySelector('h1')?.text ?? "Ingen tittel funnet";
  String avsnitt = document.querySelector('p')?.text ?? "Ingen avsnitt funnet";

  print('Tittel: $tittel');
  print('Avsnitt: $avsnitt');
}
```

Utdata:

```
Tittel: Hei, Dart!
Avsnitt: Dette er et avsnitt i et eksempel på HTML
```

For å samhandle med virkelige nettsider, kan du kombinere `html`-parsing med HTTP-forespørsler (ved å bruke `http`-pakken for å hente nettinnhold). Her er et raskt eksempel:

Først, legg til `http`-pakken sammen med `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Deretter, hent og parse en HTML-side fra nettet:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' vis parse;

void main() async {
  var url = 'https://example.com';
  
  // Hent nettsiden
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Anta at siden har <h1>-tagger du er interessert i
    var overskrifter = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Overskrifter: $overskrifter');
  } else {
    print('Forespørselen mislyktes med status: ${response.statusCode}.');
  }
}
```

Merk: Teknikken for nett-skraping som er vist ovenfor, bør brukes ansvarlig og i samsvar med nettstedets brukervilkår.
