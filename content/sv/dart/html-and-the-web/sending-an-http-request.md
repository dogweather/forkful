---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:48.094529-07:00
description: "Att skicka en HTTP-f\xF6rfr\xE5gan i Dart \xE4r processen att initiera\
  \ kommunikation med en webbserver eller API fr\xE5n en Dart-applikation. Programmerare\
  \ g\xF6r detta\u2026"
lastmod: '2024-03-13T22:44:37.608481-06:00'
model: gpt-4-0125-preview
summary: "Att skicka en HTTP-f\xF6rfr\xE5gan i Dart \xE4r processen att initiera kommunikation\
  \ med en webbserver eller API fr\xE5n en Dart-applikation."
title: "Skicka en HTTP-beg\xE4ran"
weight: 44
---

## Hur man gör:
Dart inkluderar `http`-paketet, ett kraftfullt och bekvämt sätt att arbeta med HTTP-resurser. Först inkluderar du det i din pubspec.yaml-fil:

```yaml
dependencies:
  http: ^0.13.3
```

Importera sedan det i din Dart-kod för att börja göra förfrågningar:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Svarskropp: ${response.body}');
  } else {
    print('Förfrågan misslyckades med status: ${response.statusCode}.');
  }
}
```

Exempelutdata för en framgångsrik förfrågan kan se ut så här:

```
Svarskropp: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

För mer komplexa förfrågningar, som POST-förfrågningar med en JSON-kropp, skulle du göra följande:

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
    print('Svarsstatus: ${response.statusCode}');
    print('Svarskropp: ${response.body}');
  } else {
    print('Misslyckades med att skapa ett nytt inlägg. Status: ${response.statusCode}');
  }
}
```

Exempelutdata för postförfrågan kan vara:

```
Svarsstatus: 201
Svarskropp: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Dessa exempel visar grundläggande HTTP GET- och POST-förfrågningar som använder `http`-paketet i Dart. Detta paket täcker de flesta behov för att skicka HTTP-förfrågningar, inklusive mer komplexa scenarier med huvuden och kroppsinnehåll.
