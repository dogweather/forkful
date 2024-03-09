---
title:                "Gestione degli errori"
date:                  2024-03-08T21:55:15.284290-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?
La gestione degli errori in Dart riguarda l'anticipazione e la gestione delle eccezioni che si verificano durante l'esecuzione del programma per migliorarne l'affidabilità e l'usabilità. I programmatori implementano la gestione degli errori per prevenire crash e fornire un feedback significativo agli utenti, garantendo un'esperienza applicativa più fluida e sicura.

## Come fare:
Dart supporta due tipi di errori: errori *al momento della compilazione* ed errori *al momento dell'esecuzione*. Gli errori al momento della compilazione sono rilevati dall'analizzatore di Dart prima dell'esecuzione del codice, mentre gli errori al momento dell'esecuzione, o eccezioni, si verificano durante l'esecuzione. Ecco come gestire le eccezioni in Dart:

### Try-Catch
Usa `try-catch` per catturare le eccezioni e impedire che facciano crashare la tua applicazione:

```dart
try {
    var result = 100 ~/ 0; // Tentativo di divisione per zero, lancia un'eccezione
} catch (e) {
    print('Eccezione catturata: $e'); // Gestisce l'eccezione
}
```
Output dell'esempio: `Eccezione catturata: IntegerDivisionByZeroException`

### Eccezione Specifica
Per gestire eccezioni specifiche, menziona l'eccezione dopo `catch`:

```dart
try {
    var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
    print('Impossibile dividere per zero.'); // Gestisce specificamente le eccezioni per divisione per zero
}
```
Output dell'esempio: `Impossibile dividere per zero.`

### Traccia dello Stack
Per ottenere una traccia dello stack per il debug, usa un secondo parametro nel blocco catch:

```dart
try {
    var result = 100 ~/ 0;
} catch (e, s) {
    print('Eccezione: $e');
    print('Traccia dello stack: $s'); // Stampa la traccia dello stack per il debug
}
```

### Finally
Usa `finally` per eseguire codice dopo try/catch, indipendentemente dal fatto che sia stata lanciata un'eccezione:

```dart
try {
    var result = 100 ~/ 0;
} catch (e) {
    print('Eccezione catturata: $e');
} finally {
    print('Questo viene sempre eseguito.'); // Codice di pulizia o passaggi finali
}
```
Output dell'esempio:
```
Eccezione catturata: IntegerDivisionByZeroException
Questo viene sempre eseguito.
```

### Librerie di Terze Parti
Sebbene la libreria core di Dart sia robusta per la gestione degli errori, puoi anche utilizzare pacchetti di terze parti come `dartz` per la programmazione funzionale che introduce concetti come `Either` e `Option` che possono essere usati per la gestione degli errori. Ecco un esempio che utilizza `dartz` per la gestione degli errori:

1. Aggiungi `dartz` al tuo file `pubspec.yaml` sotto le dipendenze:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Usa `Either` per gestire gli errori in modo elegante nel tuo codice Dart:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividendo, int divisore) {
    if (divisore == 0) {
        return Left('Impossibile dividere per zero.');
    } else {
        return Right(dividendo ~/ divisore);
    }
}

void main() {
    final result = divide(100, 0);
    result.fold(
        (left) => print('Errore: $left'), 
        (right) => print('Risultato: $right')
    );
}
```
Output dell'esempio: `Errore: Impossibile dividere per zero.`

La parte `Left` rappresenta generalmente l'errore, e la parte `Right` il successo. Questo schema consente di gestire gli errori in un modo più funzionale, offrendo chiarezza e controllo sulla gestione degli errori.
