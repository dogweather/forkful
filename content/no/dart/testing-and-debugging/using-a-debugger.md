---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:59.126883-07:00
description: "Hvordan: **1. Sette brytepunkter:** For \xE5 sette et brytepunkt, klikk\
  \ p\xE5 venstre marg av kodelinjen i din IDE (f.eks., Visual Studio Code eller Android\u2026"
lastmod: '2024-03-13T22:44:40.492253-06:00'
model: gpt-4-0125-preview
summary: ''
title: Bruke en debugger
weight: 35
---

## Hvordan:


### Grunnleggende feilsøking:
**1. Sette brytepunkter:**

For å sette et brytepunkt, klikk på venstre marg av kodelinjen i din IDE (f.eks., Visual Studio Code eller Android Studio) der du vil at utførelsen skal pause.

```dart
void main() {
  var melding = 'Hei, Feilsøking';
  print(melding); // Sett et brytepunkt her
}
```

**2. Starte feilsøking:**

I din IDE, start en feilsøkingsøkt ved å klikke på feilsøkingssymbolet eller trykke på feilsøkingsknappen. Utførelsen vil pause ved brytepunkter.

**3. Inspisere variabler:**

Når utførelsen er pauset, hold musepekeren over variabler for å se deres nåværende verdier.

**4. Gå gjennom kode:**

Bruk kommandoene for å steg over, steg inn i, og steg ut av i din IDE for å navigere gjennom koden din en linje eller funksjon om gangen.

### Avansert feilsøking med Observatory:
Dart inkluderer et verktøy kalt Observatory for feilsøking og profilering av Dart-applikasjoner. Det er spesielt nyttig for applikasjoner som kjører på Dart VM.

**Tilgang til Observatory:**

Kjør Dart-applikasjonen din med `--observe`-flagget.

```bash
dart --observe ditt_program.dart
```

Denne kommandoen skriver ut en URL til konsollen, som du kan åpne i en nettleser for å få tilgang til Observatory-feilsøkeren.

### Bruke populære tredjepartsbiblioteker:
For feilsøking av Flutter-applikasjoner, tilbyr `flutter_devtools`-pakken et sett med ytelses- og feilsøkingsverktøy som integrerer med både Dart VM og Flutter.

**Installasjon:**

Først, legg til `devtools` i din `pubspec.yaml`-fil under `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**Starte DevTools:**

Kjør denne kommandoen i terminalen din:

```bash
flutter pub global run devtools
```

Deretter starter du din Flutter-applikasjon i feilsøkingsmodus. DevTools tilbyr funksjoner som Flutter-inspektøren for widgettreaksanalyse og nettverksprofilering for overvåking av nettverksaktivitet.

### Eksempel på utdata:
Når du treffer et brytepunkt, kan din IDE vise variabelverdier og stakksporinger slik:

```
melding: 'Hei, Feilsøking'
```

Ved å effektivt utnytte feilsøkingsverktøy og -teknikker i Dart, kan utviklere identifisere og løse problemer raskere, noe som fører til en jevnere utviklingsprosess og mer robuste applikasjoner.
