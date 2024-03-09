---
title:                "Leggere gli argomenti della riga di comando"
date:                  2024-03-08T21:55:41.840834-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere gli argomenti della riga di comando in Dart consente ai programmatori di inserire dati direttamente nella console quando eseguono un programma Dart, aumentando l'interattività e la flessibilità per vari casi d'uso, inclusi script di automazione, strumenti CLI (interfaccia a riga di comando) o elaborazione batch. Questa caratteristica è fondamentale per creare applicazioni a riga di comando adattabili e facili da usare.

## Come fare:

Dart offre un approccio semplice per accedere agli argomenti della riga di comando tramite il `List<String> args` nel metodo principale. Di seguito è riportato un semplice esempio che mostra come leggere e utilizzare gli argomenti della riga di comando.

```dart
// main.dart
void main(List<String> args) {
  print('Argomenti della Riga di Comando:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

Per eseguire questo programma Dart e passare argomenti della riga di comando, usa il Dart CLI in questo modo:

```shell
dart run main.dart Hello World!
```

Output previsto:

```
Argomenti della Riga di Comando:
1: Hello
2: World!
```

### Utilizzo di una Popolare Libreria di Terze Parti: `args`

Sebbene le capacità integrate di Dart per la gestione degli argomenti della riga di comando siano robuste per molte applicazioni, il pacchetto `args` offre un modo più raffinato per definire e analizzare gli argomenti della riga di comando per esigenze più complesse.

Prima, aggiungi il pacchetto `args` al tuo `pubspec.yaml`:

```yaml
dependencies:
  args: ^2.0.0
```

Poi, usalo nel tuo programma come segue:

```dart
// Utilizzando il pacchetto 'args'
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('Ciao, ${argResults['name']}!');
  } else {
    print('Nessun nome fornito.');
  }
}
```

Esegui il programma con un argomento nominato:

```shell
dart run main.dart --name=John
```

Output previsto:

```
Ciao, John!
```

Questa semplice introduzione all'analisi degli argomenti della riga di comando, sia in modo nativo che con la libreria `args`, mostra come Dart possa gestire gli input degli utenti direttamente dalla console, aprendo la strada alla creazione di applicazioni CLI più interattive e dinamiche.
