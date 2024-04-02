---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:34.878241-07:00
description: "Een webpagina downloaden houdt in dat de inhoud van een webpagina via\
  \ de URL wordt opgehaald voor verwerking of opslag. Programmeurs doen dit om\u2026"
lastmod: '2024-03-13T22:44:50.503550-06:00'
model: gpt-4-0125-preview
summary: "Een webpagina downloaden houdt in dat de inhoud van een webpagina via de\
  \ URL wordt opgehaald voor verwerking of opslag. Programmeurs doen dit om\u2026"
title: Een webpagina downloaden
weight: 42
---

## Wat & Waarom?

Een webpagina downloaden houdt in dat de inhoud van een webpagina via de URL wordt opgehaald voor verwerking of opslag. Programmeurs doen dit om informatie te extraheren, wijzigingen te monitoren of inhoud te archiveren, wat het een basis maakt voor web scraping, datamining en geautomatiseerde testtaken.

## Hoe te:

Dart biedt het `http`-pakket, een populaire externe bibliotheek voor het maken van HTTP-verzoeken. Hier is een basisvoorbeeld van hoe je het kunt gebruiken om een webpagina te downloaden:

Voeg eerst het `http`-pakket toe aan je `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.3
```

Importeer vervolgens het pakket en gebruik het om de inhoud van een webpagina op te halen:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Pagina gedownload:');
    print(response.body);
  } else {
    print('Verzoek mislukt met status: ${response.statusCode}.');
  }
}
```

**Voorbeelduitvoer** (dit zal variëren op basis van de inhoud van de webpagina):

```
Pagina gedownload:
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
</html>
```

Voor complexere scenario's, zoals het omgaan met cookies of het instellen van user-agent headers, zou je hetzelfde `http`-pakket gebruiken, maar met extra configuraties voor je verzoek:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'name=value; name2=value2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('Pagina gedownload met aangepaste headers:');
    print(response.body);
  } else {
    print('Verzoek mislukt met status: ${response.statusCode}.');
  }
}
```

Het gebruik van dergelijke headers kan browserverzoeken nauwkeuriger nabootsen, wat bijzonder nuttig is bij het omgaan met sites die specifieke vereisten of beschermingen tegen scraping hebben.
