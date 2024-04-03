---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:40.999705-07:00
description: "Stampare l'output di debug in Dart riguarda la visualizzazione di informazioni\
  \ sulla console durante l'esecuzione, permettendo agli sviluppatori di\u2026"
lastmod: '2024-03-13T22:44:43.133124-06:00'
model: gpt-4-0125-preview
summary: Stampare l'output di debug in Dart riguarda la visualizzazione di informazioni
  sulla console durante l'esecuzione, permettendo agli sviluppatori di tracciare il
  flusso di esecuzione, indagare lo stato delle variabili o identificare la fonte
  degli errori.
title: Stampa dell'output di debug
weight: 33
---

## Come fare:
In Dart, puoi stampare l'output di debug utilizzando la funzione `print()`. Ecco come stampare semplici messaggi e valori delle variabili:

```dart
void main() {
  String saluto = "Ciao, Dart!";
  print(saluto); // Stampa: Ciao, Dart!

  int numero = 42;
  print('Il numero è $numero.'); // Stampa: Il numero è 42.
}
```

Per dati strutturati, come liste o oggetti, il metodo `toString()` di Dart potrebbe non fornire abbastanza dettagli. In questi casi, puoi usare la funzione `jsonEncode` della libreria `dart:convert` di Dart per convertire i dati in una stringa JSON più leggibile:

```dart
import 'dart:convert';

void main() {
  var utente = {
    'nome': 'John Doe',
    'età': 30,
    'email': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(utente));
  // Stampa: {"nome":"John Doe","età":30,"email":["john.doe@example.com","john@example.com"]}
}
```

Quando sono necessarie capacità di debug più sofisticate, come il logging con diversi livelli di importanza (info, avviso, errore), puoi utilizzare librerie di terze parti come `logger`. Ecco come usarlo:

1. Aggiungi `logger` al tuo `pubspec.yaml`:

```yaml
dependencies:
  logger: ^1.0.0
```

2. Usa `logger` nel tuo codice Dart:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("Questo è un messaggio di debug");
  logger.w("Questo è un messaggio di avviso");
  logger.e("Questo è un messaggio di errore");
}
```

L'output sarà più informativo, mostrando il livello del messaggio e il messaggio stesso, rendendo più facile distinguere tra diversi tipi di messaggi nel log.
