---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:59.920298-07:00
description: "Le espressioni regolari (regex) in Dart offrono un modo potente per\
  \ cercare e manipolare le stringhe, consentendo ai programmatori di eseguire operazioni\u2026"
lastmod: '2024-03-09T21:06:07.668341-07:00'
model: gpt-4-0125-preview
summary: "Le espressioni regolari (regex) in Dart offrono un modo potente per cercare\
  \ e manipolare le stringhe, consentendo ai programmatori di eseguire operazioni\u2026"
title: Utilizzo di espressioni regolari
---

{{< edit_this_page >}}

## Cos'è & Perché?
Le espressioni regolari (regex) in Dart offrono un modo potente per cercare e manipolare le stringhe, consentendo ai programmatori di eseguire operazioni complesse di elaborazione del testo in modo efficiente. Capendo le regex, gli sviluppatori possono eseguire validazioni del testo, cercare modelli e trasformare il testo rapidamente, il che è essenziale per l'elaborazione di moduli, l'analisi dei dati e le manipolazioni generali delle stringhe nelle applicazioni moderne.

## Come fare:
Dart utilizza la classe `RegExp` per le espressioni regolari. Ecco un esempio base per trovare un semplice modello all'interno di una stringa:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Imparare la programmazione Dart è eccitante.';

  if (pattern.hasMatch(text)) {
    print('Corrispondenza trovata!');
  } else {
    print('Nessuna corrispondenza trovata.');
  }
  // Output: Corrispondenza trovata!
}
```

Per estrarre le corrispondenze da una stringa, puoi usare il metodo `allMatches`. Questo metodo restituisce un iterabile di corrispondenze:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart è fantastico!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Questo stampa le sottostringhe corrispondenti.
  }
  // Output:
  // Dart
  // è
  // fantastico
}
```

Sostituire il testo può essere ottenuto utilizzando i metodi `replaceFirst` o `replaceAll`:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart non è solo un dardo.';
  
  // Sostituisci la prima occorrenza
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // Output: Flutter non è solo un dardo.

  // Sostituisci tutte le occorrenze
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // Output: Flutter non è solo un flutter.
}
```

Dividere una stringa mediante un modello regex è semplice usando il metodo `split`:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Corrisponde a qualsiasi carattere di spazio bianco
  var text = 'Dart è divertente';

  var parts = text.split(pattern);
  print(parts); 
  // Output: [Dart, è, divertente]
}
```

Per analisi complesse o validazioni non supportate direttamente dalla `RegExp` di Dart, potresti prendere in considerazione librerie di terze parti, ma la libreria standard di Dart è spesso sufficiente per compiti comuni con le regex, sottolineando la sua utilità e versatilità nell'uso delle espressioni regolari.
