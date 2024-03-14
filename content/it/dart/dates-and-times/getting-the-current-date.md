---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:43.856468-07:00
description: "Ottenere la data corrente in Dart comporta l'interrogazione del sistema\
  \ per conoscere la data e l'ora attuali. Questa funzionalit\xE0 \xE8 comunemente\u2026"
lastmod: '2024-03-13T22:44:43.142484-06:00'
model: gpt-4-0125-preview
summary: "Ottenere la data corrente in Dart comporta l'interrogazione del sistema\
  \ per conoscere la data e l'ora attuali. Questa funzionalit\xE0 \xE8 comunemente\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## Cosa e Perché?
Ottenere la data corrente in Dart comporta l'interrogazione del sistema per conoscere la data e l'ora attuali. Questa funzionalità è comunemente utilizzata nelle applicazioni per funzionalità come la marcatura temporale degli eventi, mostrare la data corrente agli utenti o calcolare le durate. Sapere come recuperare e manipolare efficientemente la data corrente è fondamentale per la pianificazione, la registrazione e le funzionalità sensibili al tempo.

## Come fare:
La libreria core di Dart fornisce un accesso semplice alla data e all'ora correnti attraverso la classe `DateTime`. Ecco l'esempio base per ottenere la data corrente:

```dart
void main() {
  DateTime now = DateTime.now();
  print(now); // Esempio di output: 2023-04-12 10:00:00.000
}
```

Se hai bisogno solo della parte della data (anno, mese, giorno), puoi formattare l'oggetto `DateTime`:

```dart
void main() {
  DateTime now = DateTime.now();
  String formattedDate = "${now.year}-${now.month}-${now.day}";
  print(formattedDate); // Esempio di output: 2023-04-12
}
```

Dart non include una libreria integrata per la formattazione di date più complessa, ma puoi usare il pacchetto `intl` per questo scopo. Prima, aggiungi il pacchetto al tuo `pubspec.yaml`:

```yaml
dependencies:
  intl: ^0.17.0
```

Poi, puoi formattare le date facilmente:

```dart
import 'package:intl/intl.dart';

void main() {
  DateTime now = DateTime.now();
  String formattedDate = DateFormat('yyyy-MM-dd').format(now);
  print(formattedDate); // Esempio di output: 2023-04-12
}
```

Per opzioni di formattazione più avanzate, esplora la classe `DateFormat` fornita dal pacchetto `intl`, che supporta una vasta gamma di modelli e locali.
