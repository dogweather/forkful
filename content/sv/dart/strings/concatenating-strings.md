---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:44.059142-07:00
description: "Att sammanfoga str\xE4ngar i programmering inneb\xE4r att kombinera\
  \ tv\xE5 eller flera str\xE4ngar till en. Programmerare g\xF6r detta f\xF6r att\
  \ enkelt manipulera\u2026"
lastmod: '2024-03-13T22:44:37.603266-06:00'
model: gpt-4-0125-preview
summary: "Att sammanfoga str\xE4ngar i programmering inneb\xE4r att kombinera tv\xE5\
  \ eller flera str\xE4ngar till en."
title: "Sammans\xE4ttning av str\xE4ngar"
weight: 3
---

## Hur man gör:
Dart erbjuder flera raka vägar för att sammanfoga strängar. Nedan är de vanligaste metoderna:

### Använda `+`-operatorn
`+`-operatorn är det mest intuitiva sättet att förena strängar.
```dart
String hälsning = 'Hej, ' + 'världen!';
print(hälsning); // Utdata: Hej, världen!
```

### Använda metoden `concat()`
Även om Dart inte har en `concat()`-metod liknande andra språk, kan det åstadkommas genom att använda `+` eller följande metoder.

### Använda stränginterpolering
Stränginterpolering tillåter variabler att vara direkt inbäddade inom en sträng. Det är effektivt för att kombinera strängar och uttryck.
```dart
String användare = 'Jane';
String meddelande = 'Välkommen, $användare!';
print(meddelande); // Utdata: Välkommen, Jane!
```

### Använda metoden `join()`
Metoden `join()` är användbar när du har en lista av strängar som du vill sammanfoga.
```dart
var ord = ['Hej', 'från', 'Dart'];
String mening = ord.join(' '); // Foga samman med ett mellanslag som separator.
print(mening); // Utdata: Hej från Dart
```

### Använda StringBuffer
`StringBuffer` är effektivt för flera sammanfogningar, särskilt i loopar.
```dart
var ord = ['Dart', 'är', 'kul'];
StringBuffer buffer = StringBuffer();
for (String ord in ord) {
  buffer.write(ord); // Lägg till varje ord i bufferten.
  buffer.write(' '); // Lägg valfritt till ett mellanslag.
}
String mening = buffer.toString().trim(); // Omvandla till sträng och ta bort efterföljande mellanslag.
print(mening); // Utdata: Dart är kul
```

### Tredjepartsbibliotek
Även om Darts standardbibliotek vanligtvis är tillräckligt för uppgifter som innebär sammanfogning av strängar, kan tredjepartsbibliotek som `quiver` erbjuda verktyg som kan komplettera Darts inbyggda funktionalitet. Till exempel kan `quiver`'s `concat()` eller `merge()` funktioner utforskas för avancerade scenarion. Håll dig dock till Darts robusta inbyggda alternativ om du inte har ett specifikt behov som de inte täcker.
