---
title:                "Bruke regulære uttrykk"
date:                  2024-03-08T21:56:58.312478-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk (regex) i Dart tilbyr en kraftfull måte å søke og manipulere strenger på, som gjør det mulig for programmerere å utføre komplekse tekstbehandlingsoppgaver effektivt. Ved å forstå regex, kan utviklere utføre tekstvalideringer, søkemønstre og teksttransformasjoner raskt, noe som er essensielt for behandling av skjemaer, dataparsing og generell strengmanipulasjon i moderne applikasjoner.

## Hvordan:
Dart bruker `RegExp`-klassen for regulære uttrykk. Her er et grunnleggende eksempel for å matche et enkelt mønster i en streng:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Læring av Dart-programmering er spennende.';

  if (pattern.hasMatch(text)) {
    print('Treff funnet!');
  } else {
    print('Ingen treff funnet.');
  }
  // Utdata: Treff funnet!
}
```

For å trekke ut treff fra en streng, kan du bruke `allMatches`-metoden. Denne metoden returnerer en itererbar av treff:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart er fantastisk!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // Dette skriver ut de matchende understrengene.
  }
  // Utdata:
  // Dart
  // er
  // fantastisk
}
```

Å erstatte tekst kan oppnås ved hjelp av `replaceFirst` eller `replaceAll`-metodene:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart er ikke bare en dart.';
  
  // Erstatt første forekomst
  var modifisertTekst = text.replaceFirst(pattern, 'Flutter');
  print(modifisertTekst); 
  // Utdata: Flutter er ikke bare en dart.

  // Erstatt alle forekomster
  modifisertTekst = text.replaceAll(pattern, 'Flutter');
  print(modifisertTekst);
  // Utdata: Flutter er ikke bare en flutter.
}
```

Å dele en streng ved hjelp av et regex-mønster er enkelt ved bruk av `split`-metoden:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // Matcher hvilket som helst mellomromstegn
  var text = 'Dart er gøy';

  var deler = text.split(pattern);
  print(deler); 
  // Utdata: [Dart, er, gøy]
}
```

For kompleks parsing eller valideringer som ikke støttes direkte av Darts `RegExp`, kan du vurdere tredjepartsbiblioteker, men Darts standardbibliotek er ofte tilstrekkelig for vanlige regex-oppgaver, noe som understreker dets nytte og allsidighet i behandlingen av regulære uttrykk.
