---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:55.445469-07:00
description: "Das Parsen von HTML in der Programmierung beinhaltet das Extrahieren\
  \ von Daten aus HTML-Dokumenten. Programmierer tun dies, um mit Webinhalten zu\u2026"
lastmod: '2024-03-13T22:44:53.578144-06:00'
model: gpt-4-0125-preview
summary: "Das Parsen von HTML in der Programmierung beinhaltet das Extrahieren von\
  \ Daten aus HTML-Dokumenten. Programmierer tun dies, um mit Webinhalten zu\u2026"
title: HTML Parsen
weight: 43
---

## Was & Warum?
Das Parsen von HTML in der Programmierung beinhaltet das Extrahieren von Daten aus HTML-Dokumenten. Programmierer tun dies, um mit Webinhalten zu interagieren oder Informationen für die Extraktion, das Testen oder die Automatisierung zu sammeln, selbst wenn offizielle APIs nicht verfügbar sind.

## Wie geht das:
Dart bietet in seinen Kernbibliotheken keine integrierte Unterstützung für das Parsen von HTML. Sie können jedoch ein Drittanbieter-Paket wie `html` verwenden, um HTML-Dokumente zu parsen und zu manipulieren.

Fügen Sie zunächst das `html`-Paket Ihrer `pubspec.yaml`-Datei hinzu:

```yaml
dependencies:
  html: ^0.15.0
```

Importieren Sie dann das Paket in Ihre Dart-Datei:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Hier ist ein einfaches Beispiel für das Parsen eines Strings, der HTML enthält, und das Extrahieren von Daten:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hallo, Dart!</h1>
      <p>Dies ist ein Absatz in einem Beispiel-HTML</p>
    </body>
  </html>
  """;

  // Den HTML-String parsen
  Document document = parse(htmlDocument);

  // Daten extrahieren
  String titel = document.querySelector('h1')?.text ?? "Kein Titel gefunden";
  String absatz = document.querySelector('p')?.text ?? "Kein Absatz gefunden";

  print('Titel: $titel');
  print('Absatz: $absatz');
}
```

Ausgabe:

```
Titel: Hallo, Dart!
Absatz: Dies ist ein Absatz in einem Beispiel-HTML
```

Um mit realen Webseiten zu interagieren, könnten Sie das Parsen von `html` mit HTTP-Anfragen kombinieren (indem Sie das `http`-Paket verwenden, um Webinhalte abzurufen). Hier ist ein schnelles Beispiel:

Fügen Sie zunächst das `http`-Paket zusammen mit `html` hinzu:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Holen und parsen Sie dann eine HTML-Seite aus dem Web:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Die Webseite abrufen
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Angenommen, die Seite enthält <h1>-Tags, die Sie interessieren
    var schlagzeilen = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Schlagzeilen: $schlagzeilen');
  } else {
    print('Anfrage fehlgeschlagen mit Status: ${response.statusCode}.');
  }
}
```

Hinweis: Die oben gezeigte Technik des Web Scrapings sollte verantwortungsbewusst und in Übereinstimmung mit den Nutzungsbedingungen der Website verwendet werden.
