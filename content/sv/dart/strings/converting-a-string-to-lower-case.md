---
title:                "Konvertera en sträng till gemener"
date:                  2024-03-08T21:53:57.389020-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till gemener är en grundläggande operation som innebär att man transformerar alla tecken i en given sträng till deras gemena motsvarigheter. Programmerare utför vanligtvis denna operation för att åstadkomma skiftlägesokänsliga jämförelser eller för att standardisera textinmatning för vidare bearbetning, vilket gör applikationer mer användarvänliga och data mer konsekventa.

## Hur man gör:

I Dart kan du konvertera en sträng till gemener genom att använda metoden `toLowerCase()` som tillhandahålls av `String`-klassen. Denna metod returnerar en ny sträng där alla versaler har konverterats till gemener. Låt oss se hur detta fungerar med ett enkelt exempel:

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // Utdata: hello, world!
}
```

Dart kräver inte externa bibliotek för grundläggande strängmanipulationsuppgifter, inklusive omvandling till gemener, eftersom standardbibliotekets `String`-klass är ganska omfattande. Dock, för mer komplexa manipulationer som involverar lokalspecifika regler, kan du överväga paketet `intl`, som tillhandahåller internationaliserings- och lokaliseringsfaciliteter, inklusive omvandling av skiftläge baserat på lokal:

För att använda `intl`, lägg till det i din `pubspec.yaml`-fil:

```yaml
dependencies:
  intl: ^0.17.0
```

Du kan sedan använda metoden `toLocaleLowerCase()` för att konvertera en sträng till gemener baserat på specifika lokaler:

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // Turkisk lokal
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // Utdata: istanbul
  
  // Standardlokal (en)
  print(originalString.toLowerCase()); // Utdata: i̇stanbul
}
```

I detta exempel, notera hur den turkiska lokalen korrekt hanterar det punktlösa 'i', vilket visar vikten av lokalmedvetna transformationer i internationaliserade applikationer.
