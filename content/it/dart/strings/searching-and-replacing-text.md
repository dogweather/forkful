---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:14.457336-07:00
description: "Cercare e sostituire testo in Dart comporta l'esame di stringhe per\
  \ trovare determinati schemi o sequenze di caratteri e sostituirli con nuovo contenuto.\u2026"
lastmod: '2024-03-13T22:44:43.112121-06:00'
model: gpt-4-0125-preview
summary: Cercare e sostituire testo in Dart comporta l'esame di stringhe per trovare
  determinati schemi o sequenze di caratteri e sostituirli con nuovo contenuto.
title: Ricerca e sostituzione del testo
weight: 10
---

## Cos'è e Perché?

Cercare e sostituire testo in Dart comporta l'esame di stringhe per trovare determinati schemi o sequenze di caratteri e sostituirli con nuovo contenuto. Questa operazione è fondamentale per compiti come la validazione dei dati, la formattazione dell'output, l'analisi dell'input dell'utente o anche la manipolazione di URL e percorsi di file, rendendo le applicazioni più dinamiche e reattive alle esigenze degli utenti.

## Come fare:

Dart fornisce metodi robusti per cercare e sostituire testo direttamente tramite la sua classe `String`, senza la necessità di librerie esterne. Ecco come puoi farlo:

### Ricerca e Sostituzione di Base

Per cercare una sottostringa e sostituirla con un'altra stringa, puoi usare `replaceAll`:

```dart
String sampleText = "Ciao, Dart! Dart è fantastico.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Output: Ciao, Flutter! Flutter è fantastico.
```

### Utilizzo delle Espressioni Regolari

Per esigenze di ricerca e sostituzione più complesse, Dart utilizza espressioni regolari tramite la classe `RegExp`. Questo permette la corrispondenza e la sostituzione di schemi nelle stringhe:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Output: Dart 2024, Flutter 2024
```

Questo esempio trova tutte le istanze di uno o più numeri (`\d+`) nella stringa e le sostituisce con "2024".

### Ricerca Senza Distinzione tra Maiuscole e Minuscole

Per eseguire una ricerca senza distinzione tra maiuscole e minuscole, puoi modificare il costruttore `RegExp` per ignorare il case:

```dart
String sampleText = "Benvenuti a Dart, il linguaggio di programmazione.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Output: Benvenuti a Flutter, il linguaggio di programmazione.
```

### Sostituzione con una Funzione

Per le sostituzioni dinamiche basate sulla corrispondenza stessa, Dart consente di passare una funzione a `replaceAllMapped`. Questa funzione può eseguire operazioni o calcoli sulle sequenze corrispondenti:

```dart
String sampleText = "Incrementa 5 di 1 per ottenere 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Output: Incrementa 6 di 1 per ottenere 7.
```

Questo sostituisce ogni sequenza di cifre con il suo valore incrementato. Ogni corrispondenza viene analizzata in un intero, incrementata e poi convertita nuovamente in una stringa per la sostituzione.

Le capacità di manipolazione delle stringhe di Dart, in particolare per la ricerca e la sostituzione di testo, ne fanno uno strumento potente per l'elaborazione e la preparazione dei dati all'interno delle tue applicazioni. Che tu usi sostituzioni di stringhe semplici o sfrutti la potenza delle espressioni regolari, Dart fornisce la flessibilità e le prestazioni necessarie per una manipolazione efficace del testo.
