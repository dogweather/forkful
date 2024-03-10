---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:40.224090-07:00
description: "HTTP-pyynn\xF6n l\xE4hett\xE4minen Dartissa tarkoittaa kommunikoinnin\
  \ aloittamista web-palvelimen tai API:n kanssa Dart-sovelluksesta. Ohjelmoijat tekev\xE4\
  t sen\u2026"
lastmod: '2024-03-09T21:06:20.177715-07:00'
model: gpt-4-0125-preview
summary: "HTTP-pyynn\xF6n l\xE4hett\xE4minen Dartissa tarkoittaa kommunikoinnin aloittamista\
  \ web-palvelimen tai API:n kanssa Dart-sovelluksesta. Ohjelmoijat tekev\xE4t sen\u2026"
title: "L\xE4hett\xE4m\xE4ss\xE4 HTTP-pyynt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen Dartissa tarkoittaa kommunikoinnin aloittamista web-palvelimen tai API:n kanssa Dart-sovelluksesta. Ohjelmoijat tekevät sen hakeakseen tietoa verkosta, lähettääkseen lomakkeita ja kommunikoidakseen RESTful-palveluiden kanssa, mikä tekee siitä perusoperaation web-, palvelinpuolen- ja mobiilisovelluskehityksessä Dartissa.

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
