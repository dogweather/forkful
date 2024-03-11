---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:13.875793-07:00
description: "Das Herunterladen einer Webseite beinhaltet das Abrufen des Inhalts\
  \ einer Webseite \xFCber ihre URL zur Verarbeitung oder Speicherung. Programmierer\
  \ tun\u2026"
lastmod: '2024-03-11T00:14:27.473121-06:00'
model: gpt-4-0125-preview
summary: "Das Herunterladen einer Webseite beinhaltet das Abrufen des Inhalts einer\
  \ Webseite \xFCber ihre URL zur Verarbeitung oder Speicherung. Programmierer tun\u2026"
title: Herunterladen einer Webseite
---

{{< edit_this_page >}}

## Was & Warum?

Das Herunterladen einer Webseite beinhaltet das Abrufen des Inhalts einer Webseite über ihre URL zur Verarbeitung oder Speicherung. Programmierer tun dies, um Informationen zu extrahieren, Änderungen zu überwachen oder Inhalte zu archivieren, was es zu einem Grundpfeiler beim Web Scraping, Data Mining und automatisierten Tests macht.

## Wie:

Dart stellt das `http`-Paket bereit, eine beliebte Drittanbieterbibliothek für HTTP-Anfragen. Hier ist ein grundlegendes Beispiel, wie man es verwendet, um eine Webseite herunterzuladen:

Fügen Sie zunächst das `http`-Paket zu Ihrer `pubspec.yaml` hinzu:

```yaml
dependencies:
  http: ^0.13.3
```

Dann importieren Sie das Paket und verwenden es, um den Inhalt einer Webseite abzurufen:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Seite heruntergeladen:');
    print(response.body);
  } else {
    print('Anfrage fehlgeschlagen mit Status: ${response.statusCode}.');
  }
}
```

**Beispielausgabe** (dies wird basierend auf dem Inhalt der Webseite variieren):

```
Seite heruntergeladen:
<!doctype html>
<html>
<head>
    <title>Beispiel-Domain</title>
...
</html>
```

Für komplexere Szenarien, wie das Verwalten von Cookies oder das Setzen von User-Agent-Headern, würden Sie dasselbe `http`-Paket verwenden, aber mit zusätzlichen Konfigurationen für Ihre Anfrage:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'IhrEigenerUserAgent/1.0',
    'Cookie': 'name=wert; name2=wert2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('Seite mit benutzerdefinierten Headern heruntergeladen:');
    print(response.body);
  } else {
    print('Anfrage fehlgeschlagen mit Status: ${response.statusCode}.');
  }
}
```

Das Verwenden solcher Header kann Browseranfragen genauer nachahmen, was besonders nützlich ist, wenn man mit Seiten zu tun hat, die spezifische Anforderungen oder Schutzmaßnahmen gegen Scraping haben.
