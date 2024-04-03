---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:28.873051-07:00
description: "Come fare: Dart non fornisce supporto integrato per l'analisi di HTML\
  \ nelle sue librerie core. Tuttavia, puoi utilizzare un pacchetto di terze parti\
  \ come\u2026"
lastmod: '2024-03-13T22:44:43.127404-06:00'
model: gpt-4-0125-preview
summary: Dart non fornisce supporto integrato per l'analisi di HTML nelle sue librerie
  core.
title: Analisi del HTML
weight: 43
---

## Come fare:
Dart non fornisce supporto integrato per l'analisi di HTML nelle sue librerie core. Tuttavia, puoi utilizzare un pacchetto di terze parti come `html` per analizzare e manipolare documenti HTML.

Prima, aggiungi il pacchetto `html` al tuo file `pubspec.yaml`:

```yaml
dependencies:
  html: ^0.15.0
```

Quindi, importa il pacchetto nel tuo file Dart:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

Ecco un esempio base di analisi di una stringa contenente HTML ed estrazione di dati:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Ciao, Dart!</h1>
      <p>Questo è un paragrafo in un esempio di HTML</p>
    </body>
  </html>
  """;

  // Analizza la stringa HTML
  Document document = parse(htmlDocument);

  // Estrazione di dati
  String title = document.querySelector('h1')?.text ?? "Nessun titolo trovato";
  String paragraph = document.querySelector('p')?.text ?? "Nessun paragrafo trovato";

  print('Titolo: $title');
  print('Paragrafo: $paragraph');
}
```

Output:

```
Titolo: Ciao, Dart!
Paragrafo: Questo è un paragrafo in un esempio di HTML
```

Per interagire con pagine web del mondo reale, potresti combinare l'analisi di `html` con richieste HTTP (usando il pacchetto `http` per recuperare contenuti web). Ecco un esempio veloce:

Prima, aggiungi il pacchetto `http` insieme a `html`:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

Quindi, recupera e analizza una pagina HTML dal web:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // Recupera la pagina web
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // Supponiamo che la pagina abbia tag <h1> che ti interessano
    var titoli = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('Titoli: $titoli');
  } else {
    print('Richiesta fallita con stato: ${response.statusCode}.');
  }
}
```

Nota: La tecnica di web scraping mostrata sopra dovrebbe essere utilizzata responsabilmente e nel rispetto dei termini di servizio del sito web.
