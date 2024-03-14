---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:26.546035-07:00
description: "Een HTTP-verzoek versturen met basale authenticatie houdt in dat een\
  \ gebruikersnaam en wachtwoord aan een verzoek worden toegevoegd om de identiteit\
  \ van\u2026"
lastmod: '2024-03-13T22:44:50.504528-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek versturen met basale authenticatie houdt in dat een gebruikersnaam\
  \ en wachtwoord aan een verzoek worden toegevoegd om de identiteit van\u2026"
title: Een HTTP-verzoek sturen met basisauthenticatie
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek versturen met basale authenticatie houdt in dat een gebruikersnaam en wachtwoord aan een verzoek worden toegevoegd om de identiteit van de gebruiker te verifiëren. Programmeurs gebruiken dit om toegang te krijgen tot bronnen die authenticatie vereisen, om veilige communicatie tussen de client en de server te waarborgen.

## Hoe:

In Dart kun je het `http` pakket gebruiken om HTTP-verzoeken met basale authenticatie te verzenden. Voeg eerst het `http` pakket toe aan je `pubspec.yaml` bestand:

```yaml
dependencies:
  http: ^0.13.4
```

Importeer vervolgens het pakket in je Dart-bestand:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Om een GET-verzoek met basale authenticatie te verzenden, kun je de volgende code gebruiken:

```dart
Future<void> fetchUserData() async {
  final username = 'jouwGebruikersnaam';
  final password = 'jouwWachtwoord';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://jouwapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('Gebruikersgegevens succesvol opgehaald!');
    print('Responstekst: ${response.body}');
  } else {
    print('Ophalen van gebruikersgegevens mislukt met statuscode: ${response.statusCode}');
  }
}
```

Deze code verstuurt een GET-verzoek naar 'https://jouwapi.com/userdata' met een basale authenticatieheader. De gebruikersnaam en het wachtwoord worden in base64 gecodeerd en doorgegeven in de 'Authorization'-header volgens de normen voor basale toegangsauthenticatie.

**Voorbeelduitvoer:**

Bij een succesvol verzoek en als de server een statuscode van 200 retourneert, kun je zien:

```plaintext
Gebruikersgegevens succesvol opgehaald!
Responstekst: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

Als de authenticatie faalt of er een andere fout optreedt, zal de responsstatuscode helpen bij het identificeren van het probleem.
