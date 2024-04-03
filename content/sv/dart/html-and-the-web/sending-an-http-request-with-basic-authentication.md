---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:25.884646-07:00
description: "Hur g\xF6r man: I Dart kan du anv\xE4nda `http`-paketet f\xF6r att skicka\
  \ HTTP-f\xF6rfr\xE5gningar med grundl\xE4ggande autentisering. F\xF6rst, l\xE4gg\
  \ till `http`-paketet i din\u2026"
lastmod: '2024-03-13T22:44:37.611630-06:00'
model: gpt-4-0125-preview
summary: "I Dart kan du anv\xE4nda `http`-paketet f\xF6r att skicka HTTP-f\xF6rfr\xE5\
  gningar med grundl\xE4ggande autentisering."
title: "S\xE4nda en HTTP-f\xF6rfr\xE5gan med grundl\xE4ggande autentisering"
weight: 45
---

## Hur gör man:
I Dart kan du använda `http`-paketet för att skicka HTTP-förfrågningar med grundläggande autentisering. Först, lägg till `http`-paketet i din `pubspec.yaml`-fil:

```yaml
dependencies:
  http: ^0.13.4
```

Sedan importerar du paketet i din Dart-fil:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

För att skicka en GET-förfrågan med grundläggande autentisering kan du använda följande kod:

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
    print('Användardata hämtades framgångsrikt!');
    print('Responskropp: ${response.body}');
  } else {
    print('Misslyckades med att hämta användardata med statuskod: ${response.statusCode}');
  }
}
```

Denna kod skickar en GET-förfrågan till 'https://yourapi.com/userdata' med en autentiseringsheader av grundläggande typ. Användarnamnet och lösenordet kodas i base64 och skickas i 'Authorization'-headern enligt standarderna för grundläggande åtkomstautentisering.

**Exempel på utmatning:**

Vid en framgångsrik förfrågan och om servern returnerar en statuskod på 200 kan du se:

```plaintext
Användardata hämtades framgångsrikt!
Responskropp: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

Om autentisering misslyckas eller något annat fel inträffar, kommer responsens statuskod att hjälpa till att identifiera problemet.
