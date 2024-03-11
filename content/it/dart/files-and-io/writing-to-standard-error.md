---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:59.571207-07:00
description: "Scrivere sull'errore standard (stderr) in Dart consiste nell'inviare\
  \ messaggi di errore e diagnostica a un flusso separato, distinto dall'output standard\u2026"
lastmod: '2024-03-11T00:14:16.710607-06:00'
model: gpt-4-0125-preview
summary: "Scrivere sull'errore standard (stderr) in Dart consiste nell'inviare messaggi\
  \ di errore e diagnostica a un flusso separato, distinto dall'output standard\u2026"
title: Scrittura su errore standard
---

{{< edit_this_page >}}

## Cosa & Perché?

Scrivere sull'errore standard (stderr) in Dart consiste nell'inviare messaggi di errore e diagnostica a un flusso separato, distinto dall'output standard (stdout). I programmatori fanno ciò per differenziare tra l'output normale del programma e gli errori o i messaggi di avvertimento, permettendo così un debugging e un logging più semplici.

## Come fare:

In Dart, scrivere su stderr è semplice utilizzando l'oggetto `stderr` disponibile in `dart:io`. Ecco un esempio base:

```dart
import 'dart:io';

void main() {
  stderr.writeln('Questo è un messaggio di errore.');
}
```

Output quando viene eseguito:
```
Questo è un messaggio di errore.
```
Questo messaggio viene inviato al flusso stderr, che tipicamente viene visualizzato nella console o nel terminale.

Per dimostrare una maggiore complessità, come la registrazione di un'eccezione, il ricco insieme di funzionalità di Dart permette una gestione degli errori concisa ed efficace:

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // Simula un'operazione che potrebbe generare un'eccezione
    throw Exception('Qualcosa è andato storto!');
  } catch (e) {
    stderr.writeln('Errore: $e');
  }
}

void main() {
  riskyOperation();
}
```

Output quando viene eseguito:
```
Errore: Exception: Qualcosa è andato storto!
```

Questo schema è particolarmente utile per le applicazioni che necessitano di separare i log normali dai log di errore, rendendo più facile monitorare e debuggare le applicazioni.

Anche se la libreria standard di Dart è piuttosto completa, molti programmi non richiedono librerie di terze parti per scrivere su stderr. Tuttavia, se la tua applicazione necessita di capacità di logging più sofisticate (ad esempio, su file, sulla rete, formattazione), il pacchetto `logging` è una scelta popolare. Ecco una breve occhiata all'uso di `logging` per gli errori:

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Errore Grave: È successo qualcosa di molto brutto.');
}
```

Output quando viene eseguito:
```
SEVERE: 2023-04-01 00:00:00.000: Errore Grave: È successo qualcosa di molto brutto.
```

Questo metodo offre un grado maggiore di personalizzazione e controllo su ciò che viene registrato come errore e su come viene formattato, il che può essere molto utile in applicazioni più grandi e complesse.
