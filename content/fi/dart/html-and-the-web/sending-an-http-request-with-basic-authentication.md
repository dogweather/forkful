---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:19.532955-07:00
description: "Kuinka: Dartissa voit k\xE4ytt\xE4\xE4 `http`-pakettia l\xE4hett\xE4\
  \xE4ksesi HTTP-pyynt\xF6j\xE4 perusautentikoinnilla. Lis\xE4\xE4 ensin `http`-paketti\
  \ `pubspec.yaml`-tiedostoosi."
lastmod: '2024-03-13T22:44:56.270460-06:00'
model: gpt-4-0125-preview
summary: "Dartissa voit k\xE4ytt\xE4\xE4 `http`-pakettia l\xE4hett\xE4\xE4ksesi HTTP-pyynt\xF6\
  j\xE4 perusautentikoinnilla."
title: "L\xE4hett\xE4minen HTTP-pyynt\xF6 perustodennuksella"
weight: 45
---

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
