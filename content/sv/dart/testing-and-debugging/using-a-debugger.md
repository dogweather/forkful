---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:43.358852-07:00
description: "Att anv\xE4nda en fels\xF6kare i Dart l\xE5ter programmerare metodiskt\
  \ granska sin kod genom att s\xE4tta brytpunkter, stega igenom exekveringen och\
  \ inspektera\u2026"
lastmod: '2024-03-09T21:06:02.373536-07:00'
model: gpt-4-0125-preview
summary: "Att anv\xE4nda en fels\xF6kare i Dart l\xE5ter programmerare metodiskt granska\
  \ sin kod genom att s\xE4tta brytpunkter, stega igenom exekveringen och inspektera\u2026"
title: "Att anv\xE4nda en debugger"
---

{{< edit_this_page >}}

## Vad och varför?

Att använda en felsökare i Dart låter programmerare metodiskt granska sin kod genom att sätta brytpunkter, stega igenom exekveringen och inspektera variabler. Denna process är avgörande för att identifiera och åtgärda buggar effektivt, vilket gör den till ett oumbärligt verktyg i utvecklingscykeln.

## Hur man gör:

### Grundläggande felsökning:

**1. Ställa in brytpunkter:**

För att ställa in en brytpunkt, klicka helt enkelt på vänsterkanten av koden på den rad i din IDE (t.ex. Visual Studio Code eller Android Studio) där du vill att exekveringen ska pausa.

```dart
void main() {
  var message = 'Hej, Felsökning';
  print(message); // Sätt en brytpunkt här
}
```

**2. Starta Felsökning:**

Starta en felsökningssession i din IDE genom att klicka på felsökningssymbolen eller trycka på felsökningsknappen. Exekveringen kommer att pausa vid brytpunkterna.

**3. Inspektera variabler:**

När exekveringen är pausad, håll muspekaren över variablerna för att se deras aktuella värden.

**4. Stiga igenom kod:**

Använd kommandona stega över, stega in i och stega ut ur i din IDE för att navigera genom din kod en rad eller funktion i taget.

### Avancerad felsökning med Observatory:

Dart inkluderar ett verktyg som heter Observatory för felsökning och profilering av Dart-applikationer. Det är särskilt användbart för applikationer som körs på Dart VM.

**Åtkomst till Observatory:**

Kör din Dart-applikation med flaggan `--observe`.

```bash
dart --observe ditt_program.dart
```

Detta kommando skriver ut en URL till konsolen, som du kan öppna i en webbläsare för att komma åt felsökaren Observatory.

### Använda populära tredjepartsbibliotek:

För felsökning av Flutter-applikationer, erbjuder paketet `flutter_devtools` en uppsättning prestanda- och felsökningsverktyg som integrerar med både Dart VM och Flutter.

**Installation:**

Lägg först till `devtools` i din `pubspec.yaml`-fil under `dev_dependencies`:

```yaml
dev_dependencies:
  devtools: any
```

**Starta DevTools:**

Kör detta kommando i din terminal:

```bash
flutter pub global run devtools
```

Starta sedan din Flutter-applikation i debug-läge. DevTools erbjuder funktioner som Flutter-inspektören för analys av widget-träd, och nätverksprofilern för övervakning av nätverksaktivitet.

### Exempelutskrift:

Vid träff på en brytpunkt kan din IDE visa variabelvärden och stackspårningar så här:

```
message: 'Hej, Felsökning'
```

Genom att effektivt utnyttja felsökningsverktyg och tekniker i Dart, kan utvecklare identifiera och lösa problem snabbare, vilket leder till en smidigare utvecklingsprocess och mer robusta applikationer.
