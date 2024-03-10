---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:23.634422-07:00
description: "HTML parseren in programmeren betreft het extraheren van gegevens uit\
  \ HTML-documenten. Programmeurs doen dit om te interageren met of webinhoud te\u2026"
lastmod: '2024-03-09T21:06:14.686889-07:00'
model: gpt-4-0125-preview
summary: "HTML parseren in programmeren betreft het extraheren van gegevens uit HTML-documenten.\
  \ Programmeurs doen dit om te interageren met of webinhoud te\u2026"
title: HTML Parsen
---

{{< edit_this_page >}}

## Wat & Waarom?
HTML parseren in programmeren betreft het extraheren van gegevens uit HTML-documenten. Programmeurs doen dit om te interageren met of webinhoud te schrapen voor informatie-extractie, testen of automatiseringsdoeleinden, zelfs wanneer officiële API's niet beschikbaar zijn.

## Hoe te:
Dart biedt geen ingebouwde ondersteuning voor HTML-parsing in zijn kernbibliotheken. Je kunt echter een externe package zoals `html` gebruiken om HTML-documenten te parsen en te manipuleren.

Voeg eerst de `html` package toe aan je `pubspec.yaml`-bestand:

```yaml
dependencies:
  html: ^0.15.0
```

Importeer vervolgens de package in je Dart-bestand:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Hier is een basisvoorbeeld van het parseren van een string met HTML en het extraheren van gegevens:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hallo, Dart!</h1>
      <p>Dit is een paragraaf in een voorbeeld HTML</p>
    </body>
  </html>
  """;

  // Parse de HTML-string
  Document document = parse(htmlDocument);

  // Gegevens extraheren
  String titel = document.querySelector('h1')?.text ?? "Geen titel gevonden";
  String paragraaf = document.querySelector('p')?.text ?? "Geen paragraaf gevonden";

  print('Titel: $titel');
  print('Paragraaf: $paragraaf');
}
```

Output:

```
Titel: Hallo, Dart!
Paragraaf: Dit is een paragraaf in een voorbeeld HTML
```

Om te interageren met webpagina's in de echte wereld, kun je `html`-parsing combineren met HTTP-verzoeken (gebruikmakend van de `http`-package om webinhoud op te halen). Hier is een snel voorbeeld:

Voeg eerst de `http`-package samen met `html` toe:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Vervolgens, fetch en parseer een HTML-pagina van het web:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Haal de webpagina op
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Stel je voor dat de pagina <h1>-tags heeft waarin je geïnteresseerd bent
    var koppen = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Koppen: $koppen');
  } else {
    print('Verzoek mislukt met status: ${response.statusCode}.');
  }
}
```

Let op: De hierboven getoonde web scraping-techniek moet verantwoordelijk worden gebruikt en in overeenstemming met de servicevoorwaarden van de website.
