---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:40.224090-07:00
description: "Miten: Dart sis\xE4lt\xE4\xE4 `http`-paketin, tehokkaan ja k\xE4tev\xE4\
  n tavan ty\xF6skennell\xE4 HTTP-resurssien kanssa. Ensin, sis\xE4llyt\xE4 se pubspec.yaml-tiedostoosi."
lastmod: '2024-03-13T22:44:56.267351-06:00'
model: gpt-4-0125-preview
summary: "Dart sis\xE4lt\xE4\xE4 `http`-paketin, tehokkaan ja k\xE4tev\xE4n tavan\
  \ ty\xF6skennell\xE4 HTTP-resurssien kanssa."
title: "L\xE4hett\xE4m\xE4ss\xE4 HTTP-pyynt\xF6"
weight: 44
---

## Miten:
Dart sisältää `http`-paketin, tehokkaan ja kätevän tavan työskennellä HTTP-resurssien kanssa. Ensin, sisällytä se pubspec.yaml-tiedostoosi:

```yaml
dependencies:
  http: ^0.13.3
```

Tämän jälkeen, tuo se Dart-koodiisi alkaaksesi tekemään pyyntöjä:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var vastaus = await http.get(url);

  if (vastaus.statusCode == 200) {
    print('Vastaus sisältö: ${vastaus.body}');
  } else {
    print('Pyyntö epäonnistui tilalla: ${vastaus.statusCode}.');
  }
}
```

Näyte tuloste onnistuneesta pyynnöstä voisi näyttää tältä:

```
Vastaus sisältö: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

Monimutkaisemmissa pyynnöissä, kuten POST-pyynnöissä JSON-rungolla, tekisit seuraavasti:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var vastaus = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (vastaus.statusCode == 201) {
    print('Vastaus tila: ${vastaus.statusCode}');
    print('Vastaus sisältö: ${vastaus.body}');
  } else {
    print('Uuden postauksen luonti epäonnistui. Tila: ${vastaus.statusCode}');
  }
}
```

Näyte tuloste post-pyynnöstä voisi olla:

```
Vastaus tila: 201
Vastaus sisältö: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Nämä esimerkit esittelevät perus HTTP GET ja POST -pyyntöjä käyttäen `http`-pakettia Dartissa. Tämä paketti kattaa useimmat tarpeet HTTP-pyyntöjen lähettämiseen, mukaan lukien monimutkaisemmat skenaariot otsikkojen ja rungon sisällön kanssa.
