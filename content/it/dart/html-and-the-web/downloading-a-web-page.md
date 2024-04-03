---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:38.992328-07:00
description: "Scaricare una pagina web consiste nel recuperare il contenuto di una\
  \ pagina web tramite il suo URL per elaborarlo o memorizzarlo. I programmatori fanno\u2026"
lastmod: '2024-03-13T22:44:43.128611-06:00'
model: gpt-4-0125-preview
summary: Scaricare una pagina web consiste nel recuperare il contenuto di una pagina
  web tramite il suo URL per elaborarlo o memorizzarlo.
title: Scaricare una pagina web
weight: 42
---

## Cos'è & Perché?

Scaricare una pagina web consiste nel recuperare il contenuto di una pagina web tramite il suo URL per elaborarlo o memorizzarlo. I programmatori fanno ciò per estrarre informazioni, monitorare cambiamenti o archiviare contenuti, rendendolo un elemento fondamentale nel web scraping, nel data mining e nelle attività di test automatizzati.

## Come fare:

Dart fornisce il pacchetto `http`, una popolare libreria di terze parti per effettuare richieste HTTP. Ecco un esempio base su come usarlo per scaricare una pagina web:

Prima, aggiungi il pacchetto `http` al tuo `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.3
```

Poi, importa il pacchetto e usalo per recuperare il contenuto di una pagina web:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('Pagina scaricata:');
    print(response.body);
  } else {
    print('Richiesta fallita con stato: ${response.statusCode}.');
  }
}
```

**Esempio di output** (questo varierà in base al contenuto della pagina web):

```
Pagina scaricata:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

Per scenari più complessi, come la gestione dei cookie o l'impostazione degli header user-agent, utilizzeresti lo stesso pacchetto `http` ma con configurazioni aggiuntive per la tua richiesta:

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
    print('Pagina scaricata con header personalizzati:');
    print(response.body);
  } else {
    print('Richiesta fallita con stato: ${response.statusCode}.');
  }
}
```

Utilizzare header come questi può simulare le richieste del browser in modo più accurato, il che è particolarmente utile quando si ha a che fare con siti che hanno requisiti specifici o protezioni contro lo scraping.
