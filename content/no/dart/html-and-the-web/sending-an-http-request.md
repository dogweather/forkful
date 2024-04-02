---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:28.787194-07:00
description: "\xC5 sende en HTTP-foresp\xF8rsel i Dart er prosessen med \xE5 starte\
  \ kommunikasjon med en webserver eller API fra en Dart-applikasjon. Programmerere\
  \ gj\xF8r dette\u2026"
lastmod: '2024-03-13T22:44:40.483702-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sende en HTTP-foresp\xF8rsel i Dart er prosessen med \xE5 starte kommunikasjon\
  \ med en webserver eller API fra en Dart-applikasjon. Programmerere gj\xF8r dette\u2026"
title: "Sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Hva & Hvorfor?

Å sende en HTTP-forespørsel i Dart er prosessen med å starte kommunikasjon med en webserver eller API fra en Dart-applikasjon. Programmerere gjør dette for å hente data fra nettet, sende inn skjemaer og samhandle med RESTful-tjenester, noe som gjør det til en grunnleggende operasjon for utvikling av web-, server-side- og mobilapplikasjoner i Dart.

## Hvordan:

Dart inkluderer `http`-pakken, en kraftig og praktisk måte å arbeide med HTTP-ressurser. Først inkluderer du den i din pubspec.yaml-fil:

```yaml
dependencies:
  http: ^0.13.3
```

Deretter importerer du den i Dart-koden din for å starte med å sende forespørsler:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var respons = await http.get(url);

  if (respons.statusCode == 200) {
    print('Responsinnhold: ${respons.body}');
  } else {
    print('Forespørselen feilet med status: ${respons.statusCode}.');
  }
}
```

Eksempel på utdata for en vellykket forespørsel kan se slik ut:

```
Responsinnhold: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

For mer komplekse forespørsler, som POST-forespørsler med en JSON-kropp, ville du gjøre følgende:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var respons = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (respons.statusCode == 201) {
    print('Responsstatus: ${respons.statusCode}');
    print('Responsinnhold: ${respons.body}');
  } else {
    print('Klarte ikke å opprette et nytt innlegg. Status: ${respons.statusCode}');
  }
}
```

Eksempel på utdata for postforespørselen kan være:

```
Responsstatus: 201
Responsinnhold: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

Disse eksemplene viser grunnleggende HTTP GET- og POST-forespørsler ved bruk av `http`-pakken i Dart. Denne pakken dekker de fleste behov for å sende HTTP-forespørsler, inkludert mer komplekse scenarier med headers og kroppsinhold.
