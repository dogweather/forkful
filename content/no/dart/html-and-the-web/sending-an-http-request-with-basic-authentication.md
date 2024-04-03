---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:20.530206-07:00
description: "Hvordan gj\xF8re det: I Dart kan du bruke `http`-pakken for \xE5 sende\
  \ HTTP-foresp\xF8rsler med grunnleggende autentisering. F\xF8rst, legg til `http`-pakken\
  \ i din\u2026"
lastmod: '2024-03-13T22:44:40.487032-06:00'
model: gpt-4-0125-preview
summary: "I Dart kan du bruke `http`-pakken for \xE5 sende HTTP-foresp\xF8rsler med\
  \ grunnleggende autentisering."
title: "\xC5 sende en HTTP-foresp\xF8rsel med grunnleggende autentisering"
weight: 45
---

## Hvordan gjøre det:
I Dart kan du bruke `http`-pakken for å sende HTTP-forespørsler med grunnleggende autentisering. Først, legg til `http`-pakken i din `pubspec.yaml`-fil:

```yaml
dependencies:
  http: ^0.13.4
```

Deretter importerer du pakken i din Dart-fil:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

For å sende en GET-forespørsel med grunnleggende autentisering, kan du bruke følgende kode:

```dart
Future<void> fetchUserData() async {
  final username = 'dittBrukernavn';
  final password = 'dittPassord';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://dinapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('Brukerdata hentet vellykket!');
    print('Svarinnhold: ${response.body}');
  } else {
    print('Kunne ikke hente brukerdata med statuskode: ${response.statusCode}');
  }
}
```

Denne koden sender en GET-forespørsel til 'https://dinapi.com/userdata' med en grunnleggende autentiseringsheader. Brukernavnet og passordet er kodet i base64 og passert i 'Authorization'-headeren i henhold til standardene for grunnleggende tilgangsautentisering.

**Eksempel på utdata:**

Ved vellykket forespørsel og hvis serveren returnerer en statuskode på 200, kan du se:

```plaintext
Brukerdata hentet vellykket!
Svarinnhold: {"id":1, "navn":"John Doe", "e-post":"john@example.com"}
```

Hvis autentiseringen feiler eller det oppstår en annen feil, vil responsens statuskode bidra til å identifisere problemet.
