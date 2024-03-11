---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:13.023254-07:00
description: "Inviare una richiesta HTTP in Dart \xE8 il processo di iniziazione delle\
  \ comunicazioni con un server web o API da un'applicazione Dart. I programmatori\
  \ lo\u2026"
lastmod: '2024-03-11T00:14:16.688743-06:00'
model: gpt-4-0125-preview
summary: "Inviare una richiesta HTTP in Dart \xE8 il processo di iniziazione delle\
  \ comunicazioni con un server web o API da un'applicazione Dart. I programmatori\
  \ lo\u2026"
title: Inviare una richiesta HTTP
---

{{< edit_this_page >}}

## Cosa & Perché?

Inviare una richiesta HTTP in Dart è il processo di iniziazione delle comunicazioni con un server web o API da un'applicazione Dart. I programmatori lo fanno per ottenere dati dal web, inviare moduli e interagire con servizi RESTful, rendendolo un'operazione fondamentale per lo sviluppo di applicazioni web, lato server e mobili in Dart.

## Come fare:

Dart include il pacchetto `http`, un modo potente e conveniente per lavorare con le risorse HTTP. Prima cosa, includilo nel tuo file pubspec.yaml:

```yaml
dependencies:
  http: ^0.13.3
```

Poi, importalo nel tuo codice Dart per iniziare a creare richieste:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Corpo della risposta: ${response.body}');
  } else {
    print('Richiesta fallita con stato: ${response.statusCode}.');
  }
}
```

L'output di esempio per una richiesta riuscita potrebbe essere così:

```
Corpo della risposta: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Per richieste più complesse, come le richieste POST con un corpo JSON, dovresti fare quanto segue:

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
    print('Stato della risposta: ${response.statusCode}');
    print('Corpo della risposta: ${response.body}');
  } else {
    print('Impossibile creare un nuovo post. Stato: ${response.statusCode}');
  }
}
```

L'output di esempio per la richiesta post potrebbe essere:

```
Stato della risposta: 201
Corpo della risposta: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Questi esempi mostrano le richieste HTTP GET e POST di base utilizzando il pacchetto `http` in Dart. Questo pacchetto copre la maggior parte delle necessità per l'invio di richieste HTTP, incluso scenari più complessi con intestazioni e contenuto del corpo.
