---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:29.023166-07:00
description: "Refactoring in Dart is het proces van het herstructureren van bestaande\
  \ code zonder het externe gedrag ervan te veranderen, gericht op het verbeteren\
  \ van\u2026"
lastmod: '2024-03-11T00:14:24.330353-06:00'
model: gpt-4-0125-preview
summary: "Refactoring in Dart is het proces van het herstructureren van bestaande\
  \ code zonder het externe gedrag ervan te veranderen, gericht op het verbeteren\
  \ van\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Wat & Waarom?

Refactoring in Dart is het proces van het herstructureren van bestaande code zonder het externe gedrag ervan te veranderen, gericht op het verbeteren van de interne structuur, leesbaarheid en onderhoudbaarheid. Programmeurs refactoren vaak om de code schoner, gemakkelijker te begrijpen of efficiënter te maken, wat toekomstige aanpassingen vergemakkelijkt en de kans op fouten verkleint.

## Hoe te:

### Voorbeeld 1: Hernoemen en methoden extraheren

Voor het refactoren heb je misschien een stuk code dat verschillende niveaus van abstractie of verantwoordelijkheden mengt, zoals het berekenen van een korting en vervolgens toepassen:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Eindprijs: $finalPrice");
}
```

**Uitvoer:**
```
Eindprijs: 80.0
```

Na het refactoren, kun je de kortingsberekening in zijn eigen methode extraheren en een betekenisvolle naam geven:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = berekenEindprijs(price, discount);
  print("Eindprijs: $finalPrice");
}

double berekenEindprijs(double price, double discount) {
  return price - (price * discount);
}
```

**Uitvoer:**
```
Eindprijs: 80.0
```

Door de berekening in een methode te extraheren, heb je nu een duidelijk gedefinieerde bewerking die hergebruikt, onafhankelijk getest en gemakkelijk gewijzigd kan worden.

### Voorbeeld 2: Vereenvoudigen van conditionele expressies

Voor het refactoren kunnen conditionele verklaringen overdreven complex of moeilijk te lezen zijn:

```dart
void main() {
  var klantType = "regulier";
  double korting;
  
  if (klantType == "regulier") {
    korting = 0.05;
  } else if (klantType == "lid") {
    korting = 0.1;
  } else {
    korting = 0.0;
  }

  print("Korting: $korting");
}
```

**Uitvoer:**
```
Korting: 0.05
```

Na het refactoren, overweeg het gebruik van een map voor een duidelijkere structuur en eenvoudigere updates of uitbreidingen van klanttypen en kortingen:

```dart
void main() {
  var klantType = "regulier";
  var kortingen = {
    "regulier": 0.05,
    "lid": 0.1,
    "geen": 0.0,
  };

  var korting = kortingen[klantType] ?? 0.0;
  print("Korting: $korting");
}
```

**Uitvoer:**
```
Korting: 0.05
```

Deze refactoring maakt de code niet alleen beknopter maar omvat ook de logica voor het bepalen van kortingen op een manier die gemakkelijker te begrijpen en te onderhouden is.

### Externe bibliotheken voor Refactoring

Als het gaat om refactoring in Dart, vooral binnen Flutter-apps, is de [Dart DevTools](https://dart.dev/tools/dart-devtools)-suite van onschatbare waarde. Het bevat prestatiegereedschappen, een widgetinspecteur en een bronlevel debugger. Hoewel het geen externe bibliotheek is, wordt Dart DevTools vaak gebruikt naast bibliotheken als `flutter_bloc` voor het netjes beheren van de staat op een manier die bevorderlijk is voor refactoring voor verbeterde modulariteit en leesbaarheid. Helaas, gezien de reikwijdte van deze invoer, zullen hier geen specifieke codevoorbeelden met externe bibliotheken worden gegeven, maar ontwikkelaars worden aangemoedigd om deze tools te verkennen om het refactoring-proces in hun Dart/Flutter-toepassingen te verbeteren.
