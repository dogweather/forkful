---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:39.181659-07:00
description: "Att ladda ner en webbsida inneb\xE4r att h\xE4mta inneh\xE5llet p\xE5\
  \ en webbsida via dess URL f\xF6r bearbetning eller lagring. Programmerare g\xF6\
  r detta f\xF6r att\u2026"
lastmod: '2024-03-13T22:44:37.610567-06:00'
model: gpt-4-0125-preview
summary: "Att ladda ner en webbsida inneb\xE4r att h\xE4mta inneh\xE5llet p\xE5 en\
  \ webbsida via dess URL f\xF6r bearbetning eller lagring."
title: Ladda ner en webbsida
weight: 42
---

## Vad & Varför?

Att ladda ner en webbsida innebär att hämta innehållet på en webbsida via dess URL för bearbetning eller lagring. Programmerare gör detta för att extrahera information, övervaka förändringar eller arkivera innehåll, vilket gör det till en grundsten i webbskrapning, datautvinning och automatiserade testuppgifter.

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
