---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:41.654793-07:00
description: '#'
lastmod: '2024-03-13T22:44:50.509739-06:00'
model: gpt-4-0125-preview
summary: '#'
title: Het gebruik van een debugger
weight: 35
---

## Hoe:


### Basis Debugging:
**1. Breakpoints Zetten:**

Om een breakpoint te zetten, klik simpelweg op de linker marge van de code regel in je IDE (bijv., Visual Studio Code of Android Studio) waar je de uitvoering wil pauzeren.

```dart
void main() {
  var message = 'Hallo, Debuggen';
  print(message); // Zet hier een breakpoint
}
```

**2. Start Debuggen:**

In je IDE, start een debugsessie door te klikken op het debug icoon of door de debug knop in te drukken. De uitvoering zal pauzeren bij breakpoints.

**3. Variabelen Inspecteren:**

Zodra de uitvoering is gepauzeerd, beweeg over variabelen om hun huidige waarden te zien.

**4. Door de Code Stappen:**

Gebruik de step over, step into, en step out commando's in je IDE om door je code te navigeren, één regel of functie per keer.

### Geavanceerd Debuggen met Observatory:
Dart bevat een tool genaamd Observatory voor het debuggen en profileren van Dart applicaties. Het is vooral nuttig voor applicaties die draaien op de Dart VM.

**Toegang tot Observatory:**

Draai je Dart applicatie met de `--observe` vlag.

```bash
dart --observe je_programma.dart
```

Dit commando print een URL naar de console, welke je kunt openen in een webbrowser om toegang te krijgen tot de Observatory debugger.

### Gebruik van Populaire Externe Bibliotheken:
Voor het debuggen van Flutter applicaties, biedt het `flutter_devtools` pakket een reeks van prestatie- en debugtools die integreren met zowel de Dart VM als Flutter.

**Installatie:**

Voeg eerst `devtools` toe aan je `pubspec.yaml` bestand onder `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**DevTools Starten:**

Voer dit commando in je terminal uit:

```bash
flutter pub global run devtools
```

Start dan je Flutter applicatie in debug modus. DevTools biedt functies zoals de Flutter inspector voor widgetboom analyse, en de netwerk profiler voor het monitoren van netwerkactiviteit.

### Voorbeelduitvoer:
Bij het bereiken van een breakpoint, kan je IDE variabele waarden en stacktraces weergeven zoals zo:

```
message: 'Hallo, Debuggen'
```

Door effectief gebruik te maken van debugging tools en technieken in Dart, kunnen ontwikkelaars problemen sneller identificeren en oplossen, wat leidt tot een soepeler ontwikkelingsproces en robuustere applicaties.
