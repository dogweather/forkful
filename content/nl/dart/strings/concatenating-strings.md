---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:10.380126-07:00
description: "Hoe te: Dart biedt verschillende eenvoudige manieren om strings samen\
  \ te voegen. Hieronder staan de meest voorkomende methoden: De `+` operator is de\u2026"
lastmod: '2024-03-13T22:44:50.496237-06:00'
model: gpt-4-0125-preview
summary: Dart biedt verschillende eenvoudige manieren om strings samen te voegen.
title: Strings samenvoegen
weight: 3
---

## Hoe te:
Dart biedt verschillende eenvoudige manieren om strings samen te voegen. Hieronder staan de meest voorkomende methoden:

### Gebruikmakend van de `+` Operator
De `+` operator is de meest intuïtieve manier om strings aan elkaar te verbinden.
```dart
String begroeting = 'Hallo, ' + 'Wereld!';
print(begroeting); // Uitvoer: Hallo, Wereld!
```

### Gebruikmakend van de `concat()` Methode
Hoewel Dart geen `concat()` methode heeft zoals andere talen, kan hetzelfde worden bereikt met behulp van `+` of de volgende methoden.

### Gebruikmakend van String Interpolatie
String interpolatie maakt het mogelijk om variabelen rechtstreeks in een string in te voegen. Het is efficiënt voor het combineren van strings en expressies.
```dart
String gebruiker = 'Jane';
String bericht = 'Welkom, $gebruiker!';
print(bericht); // Uitvoer: Welkom, Jane!
```

### Gebruikmakend van de `join()` Methode
De `join()` methode is handig wanneer je een lijst van strings hebt die je wilt samenvoegen.
```dart
var woorden = ['Hallo', 'van', 'Dart'];
String zin = woorden.join(' '); // Samenvoegen met een spatie als scheidingsteken.
print(zin); // Uitvoer: Hallo van Dart
```

### Gebruikmakend van StringBuffer
`StringBuffer` is efficiënt voor meerdere samenvoegingen, vooral in lussen.
```dart
var woorden = ['Dart', 'is', 'leuk'];
StringBuffer buffer = StringBuffer();
for (String woord in woorden) {
  buffer.write(woord); // Voeg elk woord toe aan de buffer.
  buffer.write(' '); // Voeg optioneel een spatie toe.
}
String zin = buffer.toString().trim(); // Omzetten naar string en de volgspatie verwijderen.
print(zin); // Uitvoer: Dart is leuk
```

### Externe Bibliotheken
Hoewel de standaardbibliotheek van Dart meestal voldoende is voor taken met betrekking tot het samenvoegen van strings, bieden externe bibliotheken zoals `quiver` hulpmiddelen die de ingebouwde functionaliteit van Dart kunnen aanvullen. Bijvoorbeeld, de `concat()` of `merge()` functies van `quiver` kunnen worden verkend voor geavanceerde scenario's. Echter, houd je aan de robuuste ingebouwde opties van Dart, tenzij je een specifieke behoefte hebt die ze niet dekken.
