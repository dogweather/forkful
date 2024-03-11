---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:18.280850-07:00
description: "Att tolka HTML i programmering inneb\xE4r att extrahera data fr\xE5\
  n HTML-dokument. Programmerare g\xF6r detta f\xF6r att interagera med eller skrapa\
  \ webbinneh\xE5ll f\xF6r\u2026"
lastmod: '2024-03-11T00:14:10.941642-06:00'
model: gpt-4-0125-preview
summary: "Att tolka HTML i programmering inneb\xE4r att extrahera data fr\xE5n HTML-dokument.\
  \ Programmerare g\xF6r detta f\xF6r att interagera med eller skrapa webbinneh\xE5\
  ll f\xF6r\u2026"
title: Tolka HTML
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML i programmering innebär att extrahera data från HTML-dokument. Programmerare gör detta för att interagera med eller skrapa webbinnehåll för informationsutvinning, testning eller automatisering, även när officiella API:er inte finns tillgängliga.

## Hur man gör:
Dart erbjuder inte inbyggt stöd för HTML-tolkning i sina kärnbibliotek. Du kan dock använda ett tredjepartspaket som `html` för att tolka och manipulera HTML-dokument.

Först, lägg till `html`-paketet i din `pubspec.yaml`-fil:

```yaml
dependencies:
  html: ^0.15.0
```

Importera sedan paketet i din Dart-fil:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Här är ett grundläggande exempel på hur man tolkar en sträng som innehåller HTML och extraherar data:

```dart
void main() {
  var htmlDokument = """
  <html>
    <body>
      <h1>Hej, Dart!</h1>
      <p>Detta är ett stycke i ett exempel på HTML</p>
    </body>
  </html>
  """;

  // Tolka HTML-strängen
  Document dokument = parse(htmlDokument);

  // Extrahera data
  String titel = dokument.querySelector('h1')?.text ?? "Ingen titel hittades";
  String stycke = dokument.querySelector('p')?.text ?? "Inget stycke hittades";

  print('Titel: $titel');
  print('Stycke: $stycke');
}
```

Utdata:

```
Titel: Hej, Dart!
Stycke: Detta är ett stycke i ett exempel på HTML
```

För att interagera med verkliga webbsidor kan du kombinera `html`-tolkning med HTTP-begäranden (använda `http`-paketet för att hämta webbinnehåll). Här är ett snabbt exempel:

Först, lägg till `http`-paketet tillsammans med `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Hämta och tolka sedan en HTML-sida från nätet:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Hämta webbsidan
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var dokument = parse(response.body);

    // Antag att sidan har <h1>-taggar du är intresserad av
    var rubriker = dokument.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Rubriker: $rubriker');
  } else {
    print('Begäran misslyckades med status: ${response.statusCode}.');
  }
}
```

Obs: Tekniken för webbskrapning som visas ovan bör användas ansvarsfullt och i enlighet med webbplatsens användarvillkor.
