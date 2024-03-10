---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:30.132624-07:00
description: "Leggere un file di testo in Dart comporta l'accesso e il recupero di\
  \ dati da file memorizzati sul file system. I programmatori effettuano questa\u2026"
lastmod: '2024-03-09T21:06:07.694458-07:00'
model: gpt-4-0125-preview
summary: "Leggere un file di testo in Dart comporta l'accesso e il recupero di dati\
  \ da file memorizzati sul file system. I programmatori effettuano questa\u2026"
title: Lettura di un file di testo
---

{{< edit_this_page >}}

## Cosa e Perché?

Leggere un file di testo in Dart comporta l'accesso e il recupero di dati da file memorizzati sul file system. I programmatori effettuano questa operazione per gestire dati di input, impostazioni di configurazione o leggere set di dati, rendendola un'operazione fondamentale per molte applicazioni, che vanno da semplici script ad app complesse.

## Come fare:

La libreria core di Dart, `dart:io`, fornisce le funzionalità necessarie per leggere file di testo sincronamente o asincronamente. Ecco come procedere in entrambi i casi.

**Sincronamente:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // Lettura del file sincronamente
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Errore nella lettura del file: $e');
  }
}
```

**Asincronamente:**

Per evitare di bloccare il programma mentre il file viene letto, particolarmente utile per file di grandi dimensioni o applicazioni reattive:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Errore nella lettura del file: $e');
  }
}
```

**Esempio di Output:**

Se il tuo file di testo contiene:

```
Hello, Dart!
```

Entrambi i metodi sopra produrranno:

```
Hello, Dart!
```

**Utilizzo di una Libreria di Terze Parti:**

Per funzionalità aggiuntive come operazioni sul file semplificate o gestione degli errori migliorata, potresti considerare librerie di terze parti come `package:file`. Tuttavia, al mio ultimo aggiornamento, l'utilizzo diretto del pacchetto core `dart:io`, come mostrato sopra, è il metodo più comune e diretto per leggere file di testo in Dart.
