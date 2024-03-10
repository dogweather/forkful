---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:16.315377-07:00
description: "Gli array associativi in Dart, comunemente noti come Mappe, sono strutture\
  \ di dati che memorizzano i dati in coppie chiave-valore. Consentono ai\u2026"
lastmod: '2024-03-09T21:06:07.671135-07:00'
model: gpt-4-0125-preview
summary: "Gli array associativi in Dart, comunemente noti come Mappe, sono strutture\
  \ di dati che memorizzano i dati in coppie chiave-valore. Consentono ai\u2026"
title: Utilizzo di array associativi
---

{{< edit_this_page >}}

## Cosa & Perché?

Gli array associativi in Dart, comunemente noti come Mappe, sono strutture di dati che memorizzano i dati in coppie chiave-valore. Consentono ai programmatori di accedere agli elementi non attraverso indici, ma chiavi, rendendo il recupero dei dati intuitivo ed efficiente, soprattutto quando si lavora con dati strutturati in cui ogni elemento ha un identificatore unico.

## Come fare:

Dart offre una sintassi semplice per creare e manipolare le Mappe. Di seguito sono riportati esempi che dimostrano operazioni di base come la creazione, l'aggiunta di elementi e il recupero di valori.

```dart
void main() {
  // Creare una mappa
  var coloriFrutta = {
    'mela': 'rosso',
    'banana': 'giallo',
    'uva': 'viola'
  };

  // Aggiungere una nuova coppia chiave-valore
  coloriFrutta['arancia'] = 'arancione';

  // Accedere a un valore tramite la sua chiave
  print(coloriFrutta['mela']); // Output: rosso

  // Aggiornare un valore
  coloriFrutta['banana'] = 'verde';

  // Iterare sulla Mappa
  coloriFrutta.forEach((frutto, colore) {
    print('$frutto: $colore');
  });
  // Output di esempio:
  // mela: rosso
  // banana: verde
  // uva: viola
  // arancia: arancione
}
```

Per strutture dati complesse o funzionalità estese, i programmatori Dart si affidano spesso a librerie aggiuntive. Una di queste librerie è `collection`, che fornisce tipi di collezione avanzati e utilità. Anche se `collection` non modifica il modo di base in cui le Mappe sono gestite, le arricchisce con funzioni di utilità e tipi di collezione più sofisticati. Ecco come potresti usarlo per un compito più specifico, come ordinare una Mappa per i suoi valori:

Prima di tutto, assicurati che il pacchetto `collection` sia incluso nel tuo file `pubspec.yaml`:

```yaml
dependencies:
  collection: ^1.15.0
```

Poi, puoi usarlo come segue:

```dart
import 'package:collection/collection.dart';

void main() {
  var coloriFrutta = {
    'mela': 'rosso',
    'banana': 'giallo',
    'uva': 'viola',
    'arancia': 'arancione'
  };

  // Ordinare la Mappa per i suoi valori (colori)
  var fruttaOrdinataPerColore = SplayTreeMap.from(
    coloriFrutta,
    (chiave1, chiave2) => coloriFrutta[chiave1]!.compareTo(coloriFrutta[chiave2]!)
  );

  print(fruttaOrdinataPerColore);
  // Output:
  // {arancia: arancione, mela: rosso, banana: giallo, uva: viola}
}
```

Questo esempio dimostra l'ordinamento delle voci di una Mappa in base ai loro valori, mostrando come Dart e il suo ecosistema vivace possano gestire agilmente gli array associativi per una manipolazione dei dati più sofisticata.
