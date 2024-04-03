---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:39.181659-07:00
description: "Hur man g\xF6r: Dart tillhandah\xE5ller paketet `http`, ett popul\xE4\
  rt tredjepartsbibliotek f\xF6r att g\xF6ra HTTP-beg\xE4ran. H\xE4r \xE4r ett grundl\xE4\
  ggande exempel p\xE5 hur man\u2026"
lastmod: '2024-03-13T22:44:37.610567-06:00'
model: gpt-4-0125-preview
summary: "Dart tillhandah\xE5ller paketet `http`, ett popul\xE4rt tredjepartsbibliotek\
  \ f\xF6r att g\xF6ra HTTP-beg\xE4ran."
title: Ladda ner en webbsida
weight: 42
---

## Hur man gör:
Dart tillhandahåller paketet `http`, ett populärt tredjepartsbibliotek för att göra HTTP-begäran. Här är ett grundläggande exempel på hur man använder det för att ladda ner en webbsida:

Först, lägg till `http`-paketet till din `pubspec.yaml`:

```yaml
beroenden:
  http: ^0.13.3
```

Sedan importerar du paketet och använder det för att hämta innehållet på en webbsida:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Sida nedladdad:');
    print(response.body);
  } else {
    print('Begäran misslyckades med status: ${response.statusCode}.');
  }
}
```

**Exempel på utdata** (detta kommer att variera baserat på innehållet på webbsidan):

```
Sida nedladdad:
<!doctype html>
<html>
<head>
    <title>Exempeldomän</title>
...
</html>
```

För mer komplexa scenarion, som att hantera cookies eller sätta användaragenthuvuden, skulle du använda samma `http`-paket men med ytterligare konfigurationer till din begäran:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'namn=värde; namn2=värde2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('Sida nedladdad med anpassade huvuden:');
    print(response.body);
  } else {
    print('Begäran misslyckades med status: ${response.statusCode}.');
  }
}
```

Att använda huvuden som dessa kan efterlikna webbläsarebegäran mer exakt, vilket är särskilt användbart när det handlar om webbplatser som har specifika krav eller skydd mot skrapning.
