---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:19.684611-07:00
description: "Het versturen van een HTTP-verzoek in Dart is het proces van het starten\
  \ van communicatie met een webserver of API vanuit een Dart-applicatie.\u2026"
lastmod: '2024-03-13T22:44:50.501425-06:00'
model: gpt-4-0125-preview
summary: "Het versturen van een HTTP-verzoek in Dart is het proces van het starten\
  \ van communicatie met een webserver of API vanuit een Dart-applicatie.\u2026"
title: Het verzenden van een HTTP-verzoek
weight: 44
---

## Wat & Waarom?

Het versturen van een HTTP-verzoek in Dart is het proces van het starten van communicatie met een webserver of API vanuit een Dart-applicatie. Programmeurs doen dit om gegevens van het web op te halen, formulieren in te dienen en te interageren met RESTful-services, waardoor het een fundamentele operatie is voor de ontwikkeling van web-, serverzijde- en mobiele applicaties in Dart.

## Hoe:

Dart bevat het `http`-pakket, een krachtige en handige manier om te werken met HTTP-bronnen. Voeg het eerst toe aan je pubspec.yaml-bestand:

```yaml
dependencies:
  http: ^0.13.3
```

Importeer het vervolgens in je Dart-code om te beginnen met het maken van verzoeken:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('Antwoord inhoud: ${response.body}');
  } else {
    print('Verzoek mislukt met status: ${response.statusCode}.');
  }
}
```

Voorbeelduitvoer voor een succesvol verzoek zou er als volgt uit kunnen zien:

```
Antwoord inhoud: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Voor meer complexe verzoeken, zoals POST-verzoeken met een JSON-body, zou je het volgende doen:

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
    print('Antwoordstatus: ${response.statusCode}');
    print('Antwoord inhoud: ${response.body}');
  } else {
    print('Mislukt om een nieuwe post te creëren. Status: ${response.statusCode}');
  }
}
```

Voorbeelduitvoer voor het postverzoek zou kunnen zijn:

```
Antwoordstatus: 201
Antwoord inhoud: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Deze voorbeelden tonen basis HTTP GET- en POST-verzoeken met behulp van het `http`-pakket in Dart. Dit pakket dekt de meeste behoeften voor het versturen van HTTP-verzoeken, inclusief complexere scenario's met headers en inhoud van de body.
