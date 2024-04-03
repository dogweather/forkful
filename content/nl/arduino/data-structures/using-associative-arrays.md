---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:07.807544-07:00
description: "Hoe: Arduino heeft strikt genomen geen ingebouwde ondersteuning voor\
  \ associatieve arrays zoals je die zou vinden in meer high-level talen. Maar, vrees\u2026"
lastmod: '2024-03-13T22:44:51.065560-06:00'
model: gpt-4-0125-preview
summary: Arduino heeft strikt genomen geen ingebouwde ondersteuning voor associatieve
  arrays zoals je die zou vinden in meer high-level talen.
title: Gebruik van associatieve arrays
weight: 15
---

## Hoe:
Arduino heeft strikt genomen geen ingebouwde ondersteuning voor associatieve arrays zoals je die zou vinden in meer high-level talen. Maar, vrees niet. We kunnen creatief worden met behulp van structuren en arrays om deze functionaliteit na te bootsen. Hier is een eenvoudig voorbeeld om een basis "associatieve array" te creëren voor het opslaan en openen van temperaturen voor verschillende steden.

Eerst, definieer een structuur om de stad (sleutel) en de temperatuur (waarde) vast te houden:

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Vervolgens, initialiseer een array van `CityTemperature` objecten:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Hier is hoe je de temperatuur van een specifieke stad kunt openen en weergeven:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("De temperatuur in Los Angeles is: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Voor nu niets hier.
}
```

Het uitvoeren van deze code zou je de uitvoer geven:

```
De temperatuur in Los Angeles is: 22.0
```

## Diepere Duik
Historisch gezien kwamen programmeertalen zoals C en C++ (waaruit de Arduino-syntax is afgeleid) niet met ingebouwde associatieve arrays, wat leidde tot work-arounds zoals die hierboven getoond. Deze benadering is relatief eenvoudig maar schaalt slecht naarmate de grootte van de data toeneemt vanwege de O(n) opzoektijd.

Talen zoals Python bieden woordenboeken, en JavaScript heeft objecten voor dit doel, die beide veel efficiënter zijn voor het beheren van sleutel-waarde paren. In Arduino, wanneer prestaties en efficiëntie kritiek worden, kunnen ontwikkelaars kiezen voor meer gespecialiseerde datastructuren, zoals hashtabellen, geïmplementeerd via bibliotheken.

Hoewel Arduino geen native ondersteuning biedt voor associatieve arrays, heeft de gemeenschap bibliotheken zoals `HashMap` ontwikkeld die aan je project kunnen worden toegevoegd om een soortgelijke functionaliteit te bieden met betere prestaties dan een doe-het-zelf-aanpak. Deze bibliotheken bieden doorgaans een elegantere en efficiëntere manier om associatieve arrays te beheren, vooral voor complexere projecten.
