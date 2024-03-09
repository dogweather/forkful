---
title:                "Konvertere en streng til små bokstaver"
date:                  2024-03-08T21:54:04.528550-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konvertere en streng til små bokstaver er en grunnleggende operasjon som involverer å transformere alle tegnene i en gitt streng til deres småbokstavtilsvarende. Programmerere utfører vanligvis denne operasjonen for å oppnå sammenligninger som er uavhengige av bokstavstørrelse, eller for å standardisere tekstinput for videre behandling, noe som gjør applikasjoner mer brukervennlige og data mer konsekvente.

## Hvordan:

I Dart kan du konvertere en streng til småbokstaver ved å bruke `toLowerCase()`-metoden som tilbys av `String`-klassen. Denne metoden returnerer en ny streng med alle store bokstaver konvertert til småbokstaver. La oss se hvordan dette fungerer med et enkelt eksempel:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Utdata: hello, world!
}
```

Dart trenger ikke eksterne biblioteker for grunnleggende manipulasjon av strenger, inkludert konvertering til småbokstaver, ettersom standardbibliotekets `String`-klasse er ganske omfattende. Imidlertid, for mer komplekse manipulasjoner som involverer regler spesifikke for lokalisering, kan du vurdere å bruke `intl`-pakken, som tilbyr internasjonalisering og lokaliseringstjenester, inkludert konvertering av bokstavstørrelse basert på lokalisering:

For å bruke `intl`, legg den til i din `pubspec.yaml`-fil:

```yaml
dependencies:
  intl: ^0.17.0
```

Deretter kan du bruke `toLocaleLowerCase()`-metoden for å konvertere en streng til småbokstaver basert på spesifikke lokaliseringer:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Tyrkisk lokalisering
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Utdata: istanbul
  
  // Standardlokalisering (en)
  print(originalString.toLowerCase()); // Utdata: i̇stanbul
}
```

I dette eksemplet, legg merke til hvordan den tyrkiske lokaliseringen korrekt behandler den prikkfrie 'i', noe som viser viktigheten av lokaliseringstilpasninger i internasjonaliserte applikasjoner.
