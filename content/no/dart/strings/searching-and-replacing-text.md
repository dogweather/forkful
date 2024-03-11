---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:38.444397-07:00
description: "S\xF8king og erstatting av tekst i Dart inneb\xE6rer \xE5 unders\xF8\
  ke strenger for \xE5 finne visse m\xF8nstre eller sekvenser av tegn og erstatte\
  \ dem med nytt innhold.\u2026"
lastmod: '2024-03-11T00:14:13.999139-06:00'
model: gpt-4-0125-preview
summary: "S\xF8king og erstatting av tekst i Dart inneb\xE6rer \xE5 unders\xF8ke strenger\
  \ for \xE5 finne visse m\xF8nstre eller sekvenser av tegn og erstatte dem med nytt\
  \ innhold.\u2026"
title: "S\xF8k og erstatt tekst"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Søking og erstatting av tekst i Dart innebærer å undersøke strenger for å finne visse mønstre eller sekvenser av tegn og erstatte dem med nytt innhold. Denne operasjonen er grunnleggende for oppgaver som datavalidering, formatering av utdata, parsing av brukerinndata, eller til og med manipulering av URL-er og filstier, noe som gjør applikasjoner mer dynamiske og responsive overfor brukerbehov.

## Hvordan:

Dart tilbyr robuste metoder for søking og erstatting av tekst direkte gjennom sin `String`-klasse, uten behov for eksterne biblioteker. Slik kan du gjøre det:

### Grunnleggende søking og erstatting

For å søke etter en delstreng og erstatte den med en annen streng, kan du bruke `replaceAll`:

```dart
String sampleText = "Hallo, Dart! Dart er flott.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Utdata: Hallo, Flutter! Flutter er flott.
```

### Bruk av regulære uttrykk

For mer komplekse søke- og erstatningsbehov, benytter Dart regulære uttrykk via `RegExp`-klassen. Dette tillater mønstersøking og erstatning i strenger:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Utdata: Dart 2024, Flutter 2024
```

Dette eksempelet finner alle forekomster av ett eller flere sifre (`\d+`) i strengen og erstatter dem med "2024".

### Søking uten hensyn til bokstavstørrelse

For å utføre et søk uten hensyn til bokstavstørrelse, kan du endre `RegExp`-konstruktøren for å ignorere bokstavstørrelse:

```dart
String sampleText = "Velkommen til Dart, programmeringsspråket.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Utdata: Velkommen til Flutter, programmeringsspråket.
```

### Erstatning med en funksjon

For dynamiske erstatninger basert på selve treffet, tillater Dart å sende en funksjon til `replaceAllMapped`. Denne funksjonen kan utføre operasjoner eller beregninger på de funnede sekvensene:

```dart
String sampleText = "Øk 5 med 1 for å få 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Utdata: Øk 6 med 1 for å få 7.
```

Dette erstatter hver siffersekvens med dens inkrementverdi. Hvert treff blir analysert til et heltall, økt og deretter konvertert tilbake til en streng for erstatning.

Darts muligheter for manipulering av strenger, spesielt for søking og erstatning av tekst, gjør det til et kraftig verktøy for behandling og forberedelse av data innenfor applikasjonene dine. Enten det er å bruke enkle strengerstatninger eller å dra nytte av kraften til regulære uttrykk, tilbyr Dart fleksibiliteten og ytelsen som er nødvendig for effektiv tekstmanipulering.
