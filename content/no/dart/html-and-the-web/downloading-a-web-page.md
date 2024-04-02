---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:32.697106-07:00
description: "\xC5 laste ned en nettside inneb\xE6rer \xE5 hente innholdet p\xE5 en\
  \ nettside via dens URL for behandling eller lagring. Programmerere gj\xF8r dette\
  \ for \xE5 trekke ut\u2026"
lastmod: '2024-03-13T22:44:40.485980-06:00'
model: gpt-4-0125-preview
summary: "\xC5 laste ned en nettside inneb\xE6rer \xE5 hente innholdet p\xE5 en nettside\
  \ via dens URL for behandling eller lagring. Programmerere gj\xF8r dette for \xE5\
  \ trekke ut\u2026"
title: Nedlasting av en nettside
weight: 42
---

## Hva & Hvorfor?

Å laste ned en nettside innebærer å hente innholdet på en nettside via dens URL for behandling eller lagring. Programmerere gjør dette for å trekke ut informasjon, overvåke endringer, eller arkivere innhold, noe som gjør det til et grunnelement i web-skraping, datautvinning og automatiserte testoppgaver.

## Hvordan:

Dart tilbyr `http`-pakken, et populært tredjepartsbibliotek for å gjøre HTTP-forespørsler. Her er et grunnleggende eksempel på hvordan man bruker det til å laste ned en nettside:

Først, legg til `http`-pakken i din `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.3
```

Deretter, importer pakken og bruk den til å hente innholdet på en nettside:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Side lastet ned:');
    print(response.body);
  } else {
    print('Forespørsel feilet med status: ${response.statusCode}.');
  }
}
```

**Eksempel på output** (dette vil variere basert på innholdet på nettsiden):

```
Side lastet ned:
<!doctype html>
<html>
<head>
    <title>Eksempeldomene</title>
...
</html>
```

For mer komplekse scenarioer, som håndtering av informasjonskapsler eller innstilling av brukeragentoverskrifter, ville du bruke samme `http`-pakke, men med ekstra konfigurasjoner til din forespørsel:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'DinEgenBrukeragent/1.0',
    'Cookie': 'navn=verdi; navn2=verdi2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('Side lastet ned med tilpassede overskrifter:');
    print(response.body);
  } else {
    print('Forespørsel feilet med status: ${response.statusCode}.');
  }
}
```

Å bruke overskrifter som disse kan etterligne nettleserforespørsler mer nøyaktig, noe som er spesielt nyttig når man har å gjøre med nettsteder som har spesifikke krav eller beskyttelser mot skraping.
