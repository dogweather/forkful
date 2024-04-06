---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:42.923024-07:00
description: "Hvordan: Dart tilbyr flere rettfram m\xE5ter \xE5 konkatenerere strenger\
  \ p\xE5. Nedenfor er de mest vanlige metodene."
lastmod: '2024-04-05T21:53:41.457039-06:00'
model: gpt-4-0125-preview
summary: "Dart tilbyr flere rettfram m\xE5ter \xE5 konkatenerere strenger p\xE5."
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Hvordan:
Dart tilbyr flere rettfram måter å konkatenerere strenger på. Nedenfor er de mest vanlige metodene:

### Bruke `+`-operatoren
`+`-operatoren er den mest intuitive måten å sammenføye strenger på.
```dart
String greeting = 'Hei, ' + 'Verden!';
print(greeting); // Utdata: Hei, Verden!
```

### Bruke `concat()`-metoden
Selv om Dart ikke har en `concat()`-metode lignende andre språk, kan det samme oppnås ved å bruke `+` eller de følgende metodene.

### Bruke strenginterpolering
Strenginterpolering lar variabler bli direkte innebygd innenfor en streng. Det er effektivt for å kombinere strenger og uttrykk.
```dart
String user = 'Jane';
String message = 'Velkommen, $user!';
print(message); // Utdata: Velkommen, Jane!
```

### Bruke `join()`-metoden
`join()`-metoden er nyttig når du har en liste med strenger som du ønsker å konkatenerere.
```dart
var words = ['Hei', 'fra', 'Dart'];
String sentence = words.join(' '); // Slår sammen med et mellomrom som separator.
print(sentence); // Utdata: Hei fra Dart
```

### Bruke StringBuffer
`StringBuffer` er effektiv for flere konkateneringer, spesielt i løkker.
```dart
var words = ['Dart', 'er', 'gøy'];
StringBuffer buffer = StringBuffer();
for (String word in words) {
  buffer.write(word); // Legger til hvert ord i bufferen.
  buffer.write(' '); // Legger valgfritt til et mellomrom.
}
String sentence = buffer.toString().trim(); // Konverterer til streng og fjerner etterfølgende mellomrom.
print(sentence); // Utdata: Dart er gøy
```

### Tredjepartsbiblioteker
Selv om Darts standardbibliotek vanligvis er tilstrekkelig for oppgaver med strengkonkatenering, tilbyr tredjepartsbiblioteker som `quiver` verktøy som kan komplementere Darts innebygde funksjonalitet. For eksempel kan `quiver` sin `concat()` eller `merge()`-funksjoner utforskes for avanserte scenarioer. Imidlertid, hold deg til Darts robuste innebygde alternativer med mindre du har et spesifikt behov som de ikke dekker.
