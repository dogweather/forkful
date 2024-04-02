---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:27.570998-07:00
description: "YAML, acronimo di YAML Ain't Markup Language, \xE8 un formato di serializzazione\
  \ dei dati leggibile dall'uomo. I programmatori lo utilizzano per file di\u2026"
lastmod: '2024-03-13T22:44:43.154189-06:00'
model: gpt-4-0125-preview
summary: "YAML, acronimo di YAML Ain't Markup Language, \xE8 un formato di serializzazione\
  \ dei dati leggibile dall'uomo. I programmatori lo utilizzano per file di\u2026"
title: Lavorare con YAML
weight: 41
---

## Cosa & Perché?

YAML, acronimo di YAML Ain't Markup Language, è un formato di serializzazione dei dati leggibile dall'uomo. I programmatori lo utilizzano per file di configurazione, scambio di dati e in applicazioni dove i dati necessitano di essere memorizzati o trasmessi in un formato facile da comprendere.

## Come fare:

In Dart, lavorare con YAML comporta tipicamente l'uso di una libreria di terze parti poiché il linguaggio non include capacità di parsing YAML integrate. Una scelta popolare è il pacchetto `yaml`. Per iniziare, è necessario aggiungere questo pacchetto al proprio `pubspec.yaml`:

```yaml
dependencies:
  yaml: ^3.1.0
```

Ricordarsi di eseguire `pub get` per recuperare il pacchetto.

### Leggere YAML

Per leggere un file YAML, innanzitutto, importare il pacchetto `yaml` e poi utilizzare la funzione `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // Output: John Doe
}

```

Assumendo che il tuo file `config.yaml` appaia così:

```yaml
name: John Doe
age: 30
```

### Scrivere YAML

Sebbene il pacchetto `yaml` sia ottimo per il parsing, non supporta la scrittura di YAML. Per questo, potrebbe essere necessario convertire manualmente i propri dati in YAML o utilizzare un altro pacchetto se disponibile. Oppure, più direttamente, gestire le trasformazioni dei dati e produrli come stringhe che corrispondono alla sintassi YAML:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // Output: name: Jane Doe
                             //         age: 29
}
```

Questo è un approccio elementare e potrebbe non essere adatto a strutture di dati complesse o a funzionalità speciali di YAML. Per esigenze sofisticate, potrebbe essere necessario cercare o contribuire a un pacchetto Dart più completo.
