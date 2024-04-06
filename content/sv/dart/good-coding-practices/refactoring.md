---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:31.329639-07:00
description: "Hur man g\xF6r: Innan refaktorering kanske du har en kodsnutt som blandar\
  \ olika abstraktionsniv\xE5er eller ansvarsomr\xE5den, som att ber\xE4kna en rabatt\
  \ och sedan\u2026"
lastmod: '2024-04-05T21:53:38.949382-06:00'
model: gpt-4-0125-preview
summary: "Innan refaktorering kanske du har en kodsnutt som blandar olika abstraktionsniv\xE5\
  er eller ansvarsomr\xE5den, som att ber\xE4kna en rabatt och sedan till\xE4mpa den."
title: Refaktorering
weight: 19
---

## Hur man gör:


### Exempel 1: Omdöpning och extrahering av metoder
Innan refaktorering kanske du har en kodsnutt som blandar olika abstraktionsnivåer eller ansvarsområden, som att beräkna en rabatt och sedan tillämpa den:

```dart
void main() {
  var pris = 100.0;
  var rabatt = 0.2;
  var slutpris = pris - (pris * rabatt);
  print("Slutpris: $slutpris");
}
```

**Output:**
```
Slutpris: 80.0
```

Efter refaktorering kan du extrahera rabattberäkningen till sin egen metod och ge den ett meningsfullt namn:

```dart
void main() {
  var pris = 100.0;
  var rabatt = 0.2;
  var slutpris = beräknaSlutpris(pris, rabatt);
  print("Slutpris: $slutpris");
}

double beräknaSlutpris(double pris, double rabatt) {
  return pris - (pris * rabatt);
}
```

**Output:**
```
Slutpris: 80.0
```

Genom att extrahera beräkningen till en metod har du nu en tydligt definierad operation som kan återanvändas, testas oberoende och enkelt modifieras.

### Exempel 2: Förenkling av villkorsuttryck
Innan refaktorering kan villkorssatser vara överdrivet komplexa eller svåra att läsa:

```dart
void main() {
  var kundtyp = "vanlig";
  double rabatt;
  
  if (kundtyp == "vanlig") {
    rabatt = 0.05;
  } else if (kundtyp == "medlem") {
    rabatt = 0.1;
  } else {
    rabatt = 0.0;
  }

  print("Rabatt: $rabatt");
}
```

**Output:**
```
Rabatt: 0.05
```

Efter refaktorering, överväg att använda en map för en tydligare struktur och enklare uppdateringar eller utvidgningar av kundtyper och rabatter:

```dart
void main() {
  var kundtyp = "vanlig";
  var rabatter = {
    "vanlig": 0.05,
    "medlem": 0.1,
    "ingen": 0.0,
  };

  var rabatt = rabatter[kundtyp] ?? 0.0;
  print("Rabatt: $rabatt");
}
```

**Output:**
```
Rabatt: 0.05
```

Denna omstrukturering gör inte bara koden mer koncis, utan kapslar också in logiken för att bestämma rabatter på ett sätt som är enklare att förstå och underhålla.

### Tredjepartsbibliotek för Refaktorering
När det gäller refaktorering i Dart, speciellt inom Flutter-appar, är [Dart DevTools](https://dart.dev/tools/dart-devtools)-sviten ovärderlig. Den inkluderar verktyg för prestanda, en widgetinspektör och en kodbaserad debugger. Även om det inte är ett tredjepartsbibliotek, används Dart DevTools ofta tillsammans med bibliotek som `flutter_bloc` för att på ett rent sätt hantera tillstånd på ett sätt som främjar refaktorering för förbättrad modularitet och läsbarhet. Tyvärr, på grund av denna texts räckvidd, kommer specifika kodexempel som använder tredjepartsbibliotek inte att tillhandahållas här, men utvecklare uppmuntras att utforska dessa verktyg för att förbättra refaktoreringsprocessen i sina Dart/Flutter-applikationer.
