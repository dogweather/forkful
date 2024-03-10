---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:16.547052-07:00
description: "\xC5 analysere en dato fra en streng i Dart inneb\xE6rer konvertering\
  \ av tekstlig representasjon av datoer og tider til et `DateTime`-objekt. Denne\
  \ operasjonen\u2026"
lastmod: '2024-03-09T21:06:05.254904-07:00'
model: gpt-4-0125-preview
summary: "\xC5 analysere en dato fra en streng i Dart inneb\xE6rer konvertering av\
  \ tekstlig representasjon av datoer og tider til et `DateTime`-objekt. Denne operasjonen\u2026"
title: Analyserer en dato fra en streng
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å analysere en dato fra en streng i Dart innebærer konvertering av tekstlig representasjon av datoer og tider til et `DateTime`-objekt. Denne operasjonen er vesentlig for applikasjoner som omhandler planlegging, dataanalyse, eller enhver funksjon som krever manipulering av datoer, for å sikre at datorelatert data blir korrekt forstått og behandlet av programmet.

## Hvordan:
Darts kjernebibliotek forenkler datoparsing gjennom `DateTime`-klassen. For greie tilfeller der du kjenner formatet på datostrengen, kan du bruke `DateTime.parse()`-metoden. Men, for mer komplekse scenarioer, eller når man håndterer flere formater, blir `intl`-pakken, spesifikt `DateFormat`-klassen, uvurderlig.

### Bruke Dart Core-biblioteket:
```dart
void main() {
  // Bruk av DateTime.parse()
  var dateString = "2023-10-31";
  var parsedDate = DateTime.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```

### Bruke `intl`-pakken:
Først, legg til `intl`-pakken i din `pubspec.yaml`-fil:
```yaml
dependencies:
  intl: ^0.17.0
```
Deretter, importer pakken og bruk `DateFormat` for parsing:
```dart
import 'package:intl/intl.dart';

void main() {
  var dateString = "October 31, 2023";
  var dateFormat = DateFormat("MMMM dd, yyyy");
  var parsedDate = dateFormat.parse(dateString);
  
  print(parsedDate); // 2023-10-31 00:00:00.000
}
```
`intl`-pakken tilbyr robuste alternativer for datoparsing, som tillater håndtering av ulike internasjonale datoformater på en sømløs måte.
