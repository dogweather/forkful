---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:19.532955-07:00
description: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla sis\xE4lt\xE4\
  \xE4 k\xE4ytt\xE4j\xE4nimen ja salasanan liitt\xE4misen pyynt\xF6\xF6n k\xE4ytt\xE4\
  j\xE4n henkil\xF6llisyyden varmistamiseksi.\u2026"
lastmod: '2024-03-13T22:44:56.270460-06:00'
model: gpt-4-0125-preview
summary: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla sis\xE4lt\xE4\xE4\
  \ k\xE4ytt\xE4j\xE4nimen ja salasanan liitt\xE4misen pyynt\xF6\xF6n k\xE4ytt\xE4\
  j\xE4n henkil\xF6llisyyden varmistamiseksi."
title: "L\xE4hett\xE4minen HTTP-pyynt\xF6 perustodennuksella"
weight: 45
---

## Mikä ja miksi?

HTTP-pyynnön lähettäminen perusautentikoinnilla sisältää käyttäjänimen ja salasanan liittämisen pyyntöön käyttäjän henkilöllisyyden varmistamiseksi. Ohjelmoijat käyttävät sitä resurssien käyttämiseen, jotka vaativat autentikointia, varmistaen turvallisen viestinnän asiakkaan ja palvelimen välillä.

## Kuinka:

Dartissa voit käyttää `http`-pakettia lähettääksesi HTTP-pyyntöjä perusautentikoinnilla. Lisää ensin `http`-paketti `pubspec.yaml`-tiedostoosi:

```yaml
dependencies:
  http: ^0.13.4
```

Tämän jälkeen tuo paketti Dart-tiedostoosi:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Perusautentikoinnilla varustetun GET-pyynnön lähettämiseksi voit käyttää seuraavaa koodia:

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('Käyttäjätiedot noudettu onnistuneesti!');
    print('Vastauksen sisältö: ${response.body}');
  } else {
    print('Käyttäjätietojen nouto epäonnistui, tilakoodi: ${response.statusCode}');
  }
}
```

Tämä koodi lähettää GET-pyynnön osoitteeseen 'https://yourapi.com/userdata' perusautentikointiotsikolla. Käyttäjänimi ja salasana koodataan base64-muotoon ja lähetetään 'Authorization'-otsikossa peruspääsyautentikoinnin standardien mukaisesti.

**Esimerkkituloste:**

Onnistuneen pyynnön ja palvelimen palauttaman tilakoodin 200 yhteydessä saatat nähdä:

```plaintext
Käyttäjätiedot noudettu onnistuneesti!
Vastauksen sisältö: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

Jos autentikointi epäonnistuu tai muu virhe ilmenee, vastauksen tilakoodi auttaa tunnistamaan ongelman.
