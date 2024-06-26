---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:11.539831-07:00
description: "Wie geht das: Dart beinhaltet das `http` Paket, eine leistungsstarke\
  \ und bequeme M\xF6glichkeit, mit HTTP-Ressourcen zu arbeiten. Zuerst inkludiere\
  \ es in\u2026"
lastmod: '2024-03-13T22:44:53.577112-06:00'
model: gpt-4-0125-preview
summary: "Dart beinhaltet das `http` Paket, eine leistungsstarke und bequeme M\xF6\
  glichkeit, mit HTTP-Ressourcen zu arbeiten."
title: Eine HTTP-Anfrage senden
weight: 44
---

## Wie geht das:
Dart beinhaltet das `http` Paket, eine leistungsstarke und bequeme Möglichkeit, mit HTTP-Ressourcen zu arbeiten. Zuerst inkludiere es in deiner pubspec.yaml-Datei:

```yaml
dependencies:
  http: ^0.13.3
```

Importiere es dann in deinen Dart-Code, um mit dem Senden von Anfragen zu beginnen:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Antwortinhalt: ${response.body}');
  } else {
    print('Anfrage fehlgeschlagen mit Status: ${response.statusCode}.');
  }
}
```

Beispiel-Ausgabe für eine erfolgreiche Anfrage könnte so aussehen:

```
Antwortinhalt: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Für komplexere Anfragen, wie POST-Anfragen mit einem JSON-Körper, würdest du Folgendes tun:

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
    print('Antwortstatus: ${response.statusCode}');
    print('Antwortinhalt: ${response.body}');
  } else {
    print('Erstellung eines neuen Posts fehlgeschlagen. Status: ${response.statusCode}');
  }
}
```

Beispiel-Ausgabe für die POST-Anfrage könnte sein:

```
Antwortstatus: 201
Antwortinhalt: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Diese Beispiele zeigen grundlegende HTTP GET- und POST-Anfragen unter Verwendung des `http`-Pakets in Dart. Dieses Paket deckt die meisten Bedürfnisse für das Senden von HTTP-Anfragen ab, einschließlich komplexerer Szenarien mit Headern und Inhaltskörpern.
