---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:33.068074-07:00
description: "Hvordan: Darts kjernebibliotek, `dart:io`, gir de n\xF8dvendige funksjonene\
  \ for \xE5 lese tekstfiler synkront eller asynkront. Her er hvordan man kan n\xE6\
  rme seg\u2026"
lastmod: '2024-03-13T22:44:40.506756-06:00'
model: gpt-4-0125-preview
summary: "Darts kjernebibliotek, `dart:io`, gir de n\xF8dvendige funksjonene for \xE5\
  \ lese tekstfiler synkront eller asynkront."
title: Lese en tekstfil
weight: 22
---

## Hvordan:
Darts kjernebibliotek, `dart:io`, gir de nødvendige funksjonene for å lese tekstfiler synkront eller asynkront. Her er hvordan man kan nærme seg begge.

**Synkront:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // Leser filen synkront
  var innhold;
  try {
    innhold = file.readAsStringSync();
    print(innhold);
  } catch (e) {
    print('Feil ved lesing av fil: $e');
  }
}
```

**Asynkront:**

For å unngå å blokkere programmet mens filen leses, spesielt nyttig for store filer eller responsive applikasjoner:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String innhold = await file.readAsString();
    print(innhold);
  } catch (e) {
    print('Feil ved lesing av fil: $e');
  }
}
```

**Eksempel på utdata:**

Hvis tekstfilen din inneholder:

```
Hello, Dart!
```

Vil begge ovennevnte metoder gi utdata:

```
Hello, Dart!
```

**Bruk av et tredjepartsbibliotek:**

For ekstra funksjoner som forenklede filoperasjoner eller forbedret feilhåndtering, kan man vurdere tredjepartsbiblioteker som `package:file`. Men, per min siste oppdatering, er bruken av det kjernepakken `dart:io` direkte, som vist ovenfor, den mest vanlige og rettfram metoden for å lese tekstfiler i Dart.
