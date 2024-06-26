---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:34.464910-07:00
description: "Wie: In Dart k\xF6nnen Sie das `http`-Paket verwenden, um HTTP-Anfragen\
  \ mit Basisauthentifizierung zu senden. F\xFCgen Sie zun\xE4chst das `http`-Paket\
  \ zu Ihrer\u2026"
lastmod: '2024-03-13T22:44:53.580243-06:00'
model: gpt-4-0125-preview
summary: "In Dart k\xF6nnen Sie das `http`-Paket verwenden, um HTTP-Anfragen mit Basisauthentifizierung\
  \ zu senden."
title: Eine HTTP-Anfrage mit Basisauthentifizierung senden
weight: 45
---

## Wie:
In Dart können Sie das `http`-Paket verwenden, um HTTP-Anfragen mit Basisauthentifizierung zu senden. Fügen Sie zunächst das `http`-Paket zu Ihrer `pubspec.yaml`-Datei hinzu:

```yaml
dependencies:
  http: ^0.13.4
```

Importieren Sie dann das Paket in Ihre Dart-Datei:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Um eine GET-Anfrage mit Basisauthentifizierung zu senden, können Sie den folgenden Code verwenden:

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
    print('Benutzerdaten erfolgreich abgerufen!');
    print('Antworttext: ${response.body}');
  } else {
    print('Fehler beim Abrufen der Benutzerdaten mit Statuscode: ${response.statusCode}');
  }
}
```

Dieser Code sendet eine GET-Anfrage an 'https://yourapi.com/userdata' mit einem Basic-Authentifizierung-Header. Der Benutzername und das Passwort werden in base64 kodiert und im 'Authorization'-Header gemäß den Standards der Basiszugangsauthentifizierung übergeben.

**Beispielausgabe:**

Bei einer erfolgreichen Anfrage und wenn der Server einen Statuscode von 200 zurückgibt, könnte man sehen:

```plaintext
Benutzerdaten erfolgreich abgerufen!
Antworttext: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

Wenn die Authentifizierung fehlschlägt oder ein anderer Fehler auftritt, hilft der Antwortstatuscode, das Problem zu identifizieren.
