---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:19.007172-07:00
description: "Att s\xF6ka och ers\xE4tta text i Dart inneb\xE4r att unders\xF6ka str\xE4\
  ngar f\xF6r att hitta vissa m\xF6nster eller teckenf\xF6ljder och substituera dem\
  \ med nytt inneh\xE5ll.\u2026"
lastmod: '2024-03-13T22:44:37.595056-06:00'
model: gpt-4-0125-preview
summary: "Att s\xF6ka och ers\xE4tta text i Dart inneb\xE4r att unders\xF6ka str\xE4\
  ngar f\xF6r att hitta vissa m\xF6nster eller teckenf\xF6ljder och substituera dem\
  \ med nytt inneh\xE5ll."
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

## Vad och varför?

Att söka och ersätta text i Dart innebär att undersöka strängar för att hitta vissa mönster eller teckenföljder och substituera dem med nytt innehåll. Denna operation är grundläggande för uppgifter som data validering, formatering av utskrift, tolkning av användarinput, eller till och med manipulering av URL:er och färdvägar för filer, vilket gör applikationer mer dynamiska och lyhörda för användarbehov.

## Hur gör man:

Dart tillhandahåller robusta metoder för att söka och ersätta text direkt genom sin `String`-klass, utan behov av externa bibliotek. Så här kan du göra det:

### Grundläggande Sökning och Ersättning

För att söka efter en delsträng och ersätta den med en annan sträng kan du använda `replaceAll`:

```dart
String sampleText = "Hej, Dart! Dart är toppen.";
String modifiedText = sampleText.replaceAll("Dart", "Flutter");
print(modifiedText); // Utskrift: Hej, Flutter! Flutter är toppen.
```

### Använda Reguljära Uttryck

För mer komplexa sök- och ersättningsbehov utnyttjar Dart reguljära uttryck via `RegExp`-klassen. Detta möjliggör mönstersökning och ersättning i strängar:

```dart
String sampleText = "Dart 2023, Flutter 2023";
String modifiedText = sampleText.replaceAll(RegExp(r'\d+'), "2024");
print(modifiedText); // Utskrift: Dart 2024, Flutter 2024
```

Det här exemplet hittar alla instanser av en eller flera siffror (`\d+`) i strängen och ersätter dem med "2024".

### Skiftlägesokänslig Sökning

För att utföra en skiftlägesokänslig sökning kan du modifiera `RegExp`-konstruktören för att ignorera skiftläget:

```dart
String sampleText = "Välkommen till Dart, programmeringsspråket.";
String modifiedText = sampleText.replaceAll(RegExp(r'dart', caseSensitive: false), "Flutter");
print(modifiedText); // Utskrift: Välkommen till Flutter, programmeringsspråket.
```

### Ersättning med en Funktion

För dynamiska ersättningar baserade på själva matchningen tillåter Dart att passera en funktion till `replaceAllMapped`. Denna funktion kan utföra operationer eller beräkningar på de matchade sekvenserna:

```dart
String sampleText = "Öka 5 med 1 för att få 6.";
String incrementedText = sampleText.replaceAllMapped(RegExp(r'\d+'), (Match m) => (int.parse(m[0]!) + 1).toString());
print(incrementedText); // Utskrift: Öka 6 med 1 för att få 7.
```

Detta ersätter varje siffersekvens med dess inkrementvärde. Varje match omvandlas till ett heltal, inkrementeras, och konverteras sedan tillbaka till en sträng för ersättning.

Darts förmågor att manipulera strängar, särskilt för att söka och ersätta text, gör det till ett kraftfullt verktyg för att behandla och förbereda data inom dina applikationer. Oavsett om du använder raka strängersättningar eller utnyttjar kraften i reguljära uttryck, erbjuder Dart den flexibilitet och prestanda som krävs för effektiv textmanipulering.
