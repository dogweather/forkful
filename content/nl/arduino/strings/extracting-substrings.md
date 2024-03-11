---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:27.892968-07:00
description: "Substrings extraheren betekent specifieke delen uit een string halen\
  \ - zoals het nemen van een stuk taart. Programmeurs doen dit om gegevens te isoleren,\u2026"
lastmod: '2024-03-11T00:14:24.895737-06:00'
model: gpt-4-0125-preview
summary: "Substrings extraheren betekent specifieke delen uit een string halen - zoals\
  \ het nemen van een stuk taart. Programmeurs doen dit om gegevens te isoleren,\u2026"
title: Substrings extraheren
---

{{< edit_this_page >}}

## Wat & Waarom?

Substrings extraheren betekent specifieke delen uit een string halen - zoals het nemen van een stuk taart. Programmeurs doen dit om gegevens te isoleren, invoer te schonen of tekst te manipuleren voor zaken zoals het analyseren van berichten van sensoren.

## Hoe:

Met Arduino strings kun je segmenten snijden en verdelen met `substring()`:

```arduino
void setup() {
  Serial.begin(9600);
  String zin = "Hallo, Arduino Wereld!";
  String begroeting = zin.substring(0, 5);
  String locatie = zin.substring(7, 19);
  
  Serial.println(begroeting); // Drukt "Hallo" af
  Serial.println(locatie); // Drukt "Arduino Wereld" af
}

void loop() {
  // Niets om hier te herhalen.
}
```

Uitvoer op Seriële Monitor:
```
Hallo
Arduino Wereld
```

## Diepere Duik

Lang voordat Arduino het simpel maakte, gebruikten programmeurs char arrays en functies zoals `strncpy` in C. Ze zijn niet alleen historische overblijfselen, maar worden nog steeds gebruikt voor lagere-niveau operaties. De functie `substring()` in Arduino is eigenlijk een wrapper die het voor ons gemakkelijker maakt bij het omgaan met String-objecten. Maar wees je bewust, het gebruik van `String` kan leiden tot geheugenfragmentatie. Als stabiliteit cruciaal is, vooral in langlopende of complexe programma's, overweeg dan de oude methoden van `char` arrays.

Alternatieven voor `substring()` omvatten directe manipulatie van char arrays of functies zoals `strtok()`. Deze kunnen efficiënter zijn maar kunnen je achterlaten met meer code om te beheren.

Intern creëert `substring()` een nieuw String-object dat de karakters bevat vanaf de startindex tot net voor de eindindex, die kan worden weggelaten als je alles tot het einde wilt.

## Zie Ook:

- Arduino String Referentie: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Geheugenbeheer in Arduino: https://learn.arduino.cc/programming/variables-and-data-types/memory-management
- C++ `std::string` substr methode, ter vergelijking: http://www.cplusplus.com/reference/string/string/substr/
