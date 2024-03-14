---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:38.525140-07:00
description: "Un interprete interattivo (REPL - Read-Evaluate-Print Loop) per Dart\
  \ consente ai programmatori di digitare e eseguire dinamicamente il codice Dart\
  \ riga\u2026"
lastmod: '2024-03-13T22:44:43.132045-06:00'
model: gpt-4-0125-preview
summary: "Un interprete interattivo (REPL - Read-Evaluate-Print Loop) per Dart consente\
  \ ai programmatori di digitare e eseguire dinamicamente il codice Dart riga\u2026"
title: Utilizzo di un guscio interattivo (REPL)
---

{{< edit_this_page >}}

## Cosa & Perché?

Un interprete interattivo (REPL - Read-Evaluate-Print Loop) per Dart consente ai programmatori di digitare e eseguire dinamicamente il codice Dart riga per riga senza la necessità di compilare interi script. Questo strumento è inestimabile per imparare la sintassi di Dart, sperimentare con snippet di codice o fare debug offrendo feedback immediati e facilitando test iterativi.

## Come fare:

Dart non viene fornito con un REPL integrato. Tuttavia, è possibile ottenere funzionalità simili a un REPL utilizzando DartPad (online) o utilizzando strumenti di terze parti come `dart_repl`.

**Utilizzando DartPad:**

DartPad (https://dartpad.dev) è un editor Dart online che ti consente di scrivere ed eseguire codice Dart nel tuo browser web. Anche se non è un REPL tradizionale a riga di comando, offre un'esperienza simile per la sperimentazione rapida.

Basta andare sul sito web, digitare il codice Dart nel pannello di sinistra e fare clic su "Run" per vedere l'output a destra.

Esempio:
```dart
void main() {
  print('Ciao, Dart!');
}
```
Output:
```
Ciao, Dart!
```

**Utilizzando `dart_repl` (strumento di terze parti):**

Prima, installa `dart_repl` tramite pub globalmente:

```shell
dart pub global activate dart_repl
```

Poi, esegui `dart_repl` dal tuo terminale:

```shell
dart_repl
```

Ora, puoi iniziare a digitare direttamente nel shell istruzioni Dart. Ad esempio:

```dart
>>> print('Ciao, REPL!');
Ciao, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

Questi metodi forniscono un percorso rapido per provare il codice Dart al volo, facilitando notevolmente la curva di apprendimento e aumentando la produttività.
