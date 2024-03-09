---
title:                "Fjerning av anførselstegn fra en streng"
date:                  2024-03-08T21:56:36.318482-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å fjerne anførselstegn fra en streng i Dart involverer å stripe bort doble (") eller enkle (') anførselstegn fra starten og slutten av en streng, nyttig for rensing av data eller forberedelse av strenger for videre behandling. Programmerere gjør dette for å normalisere datainnganger, sikre uniformitet i datalagring, eller når de grensesnitt med API-er som kan returnere data i siterte formater.

## Hvordan:
Dart gir greie måter å fjerne anførselstegn fra en streng på ved å bruke innebygde strengmetoder uten behov for tredjeparts biblioteker.

### Eksempel 1: Bruke `replaceFirst` og `replaceAll`
Hvis du håndterer strenger som starter og slutter med anførselstegn, kan du bruke `replaceFirst` og `replaceAll` metoder for å fjerne dem.

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// Fjerne doble anførselstegn
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // Utgang: Hello, World!

// Fjerne enkle anførselstegn
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // Utgang: Dart Programming
```

### Eksempel 2: Bruke `substring`
Denne metoden er nyttig når du er sikker på at anførselstegnene er helt i starten og slutten av strengen.

```dart
String quotedString = '"Flutter Development"';
// Sjekk om den starter og slutter med anførselstegn før fjerning for å unngå feil
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // Utgang: Flutter Development
```

### Eksempel 3: Egendefinert utvidelsesmetode
For mer gjenbrukbarhet, spesielt hvis ditt prosjekt involverer hyppig fjerning av sitater, vurder å opprette en egendefinert utvidelse på `String`.

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
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // Utgang: This is Dart
  print(singleQuoted.unquote()); // Utgang: This is awesome
}
```

Disse tilnærmingene bør hjelpe deg med å effektivt fjerne anførselstegn fra strenger i Dart, og forbedre dine dataforedlings- og forberedelsesarbeidsflyter.
