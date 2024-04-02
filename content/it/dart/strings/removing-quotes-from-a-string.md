---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:16.142384-07:00
description: "Rimuovere le virgolette da una stringa in Dart comporta l'eliminazione\
  \ dei segni di virgolettatura doppi (\") o singoli (') dall'inizio e dalla fine\
  \ di una\u2026"
lastmod: '2024-03-13T22:44:43.116000-06:00'
model: gpt-4-0125-preview
summary: "Rimuovere le virgolette da una stringa in Dart comporta l'eliminazione dei\
  \ segni di virgolettatura doppi (\") o singoli (') dall'inizio e dalla fine di una\u2026"
title: Rimuovere le virgolette da una stringa
weight: 9
---

## Cosa e perché?
Rimuovere le virgolette da una stringa in Dart comporta l'eliminazione dei segni di virgolettatura doppi (") o singoli (') dall'inizio e dalla fine di una stringa, utile per la pulizia dei dati o per preparare le stringhe per un ulteriore elaborazione. I programmatori lo fanno per normalizzare gli input dei dati, garantire l'uniformità nella memorizzazione dei dati o quando si interfacciano con API che possono restituire dati in formati quotati.

## Come fare:
Dart offre modi semplici per rimuovere le virgolette da una stringa utilizzando metodi di stringa incorporati senza la necessità di librerie di terze parti.

### Esempio 1: Usando `replaceFirst` e `replaceAll`
Se stai lavorando con stringhe che iniziano e finiscono con virgolette, puoi utilizzare i metodi `replaceFirst` e `replaceAll` per rimuoverle.

```dart
String quotedString = '"Ciao, Mondo!"';
String singleQuotedString = '\'Programmazione Dart\'';

// Rimozione delle virgolette doppie
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Output: Ciao, Mondo!

// Rimozione delle virgolette singole
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Output: Programmazione Dart
```

### Esempio 2: Usando `substring`
Questo metodo è utile quando sei sicuro che le virgolette siano all'inizio e alla fine della stringa.

```dart
String quotedString = '"Sviluppo Flutter"';
// Verifica se inizia e finisce con virgolette prima di rimuovere per evitare errori
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Output: Sviluppo Flutter
```

### Esempio 3: Metodo di Estensione Personalizzato
Per maggiore riutilizzabilità, in particolare se il tuo progetto comporta la frequente rimozione di virgolette, prendi in considerazione la creazione di un'estensione personalizzata su `String`.

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"Questo è Dart"';
  String singleQuoted = '\'Questo è fantastico\'';
  print(doubleQuoted.unquote()); // Output: Questo è Dart
  print(singleQuoted.unquote()); // Output: Questo è fantastico
}
```

Questi approcci dovrebbero aiutarti a rimuovere efficacemente le virgolette dalle stringhe in Dart, migliorando i tuoi flussi di lavoro di elaborazione e preparazione dei dati.
