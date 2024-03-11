---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:46.113682-07:00
description: "\xC5 skrive en tekstfil i Dart inneb\xE6rer \xE5 opprette eller modifisere\
  \ filer p\xE5 disken for \xE5 lagre data i et leselig format. Programmerere gj\xF8\
  r dette for \xE5\u2026"
lastmod: '2024-03-11T00:14:14.037376-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive en tekstfil i Dart inneb\xE6rer \xE5 opprette eller modifisere\
  \ filer p\xE5 disken for \xE5 lagre data i et leselig format. Programmerere gj\xF8\
  r dette for \xE5\u2026"
title: Skrive en tekstfil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive en tekstfil i Dart innebærer å opprette eller modifisere filer på disken for å lagre data i et leselig format. Programmerere gjør dette for å lagre applikasjonsdata, konfigurasjoner, logger eller annen informasjon som skal vedvare mellom applikasjonskjøringer eller dele data med andre applikasjoner eller brukere.

## Hvordan gjøre det:
Darts kjernebibliotek tilbyr `dart:io`-pakken for filhåndtering, som lar deg skrive tekstfiler uten behov for tredjeparts biblioteker. Her er et enkelt eksempel på å skrive en tekstfil:

```dart
import 'dart:io';

void main() async {
  // Oppretter en ny fil med navn 'example.txt' i gjeldende mappe.
  var file = File('example.txt');
  
  // Skriver en streng til filen.
  await file.writeAsString('Hei, Dart!');
  
  // Bekrefte innholdet.
  print(await file.readAsString()); // Utdata: Hei, Dart!
}
```

Når du håndterer større filer eller strømmer av data, kan du foretrekke å skrive innhold ved å bruke `openWrite`, som returnerer en `IOSink` og lar deg skrive data i deler:

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // Skriver flere linjer til filen.
  sink
    ..writeln('Linje 1: Den raske brune reven hopper over den late hunden.')
    ..writeln('Linje 2: Dart er fantastisk!')
    ..close();

  // Vent på at sinken skal lukkes for å sikre at alle data er skrevet til filen.
  await sink.done;

  // Les og skriv ut filinnhold for å bekrefte
  print(await file.readAsString());
}
```

For mer avanserte filoperasjoner, inkludert å legge til filer eller skrive bytes, kan du fordype deg mer i `File`-klassemetodene som tilbys av `dart:io`. I tillegg, når du jobber med større skala eller mer komplekse prosjekter, kan det være nyttig å vurdere pakker som `path` for håndtering av filstier eller `shelf` for funksjonaliteten til webservere, selv om direkte filskriving vanligvis støtter seg på de innebygde Dart-bibliotekene.
