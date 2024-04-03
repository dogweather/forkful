---
date: 2024-01-20 17:45:23.705683-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:56.814100-06:00'
model: gpt-4-1106-preview
summary: .
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to:
```Arduino
String alkuperainen = "Moikka Suomi";
String osajono = alkuperainen.substring(0, 6);

void setup() {
  // Aloita sarjaliikenne kommunikointi nopeudella 9600 bps.
  Serial.begin(9600);
}

void loop() {
  // Tulosta osajono sarjaliikenteeseen
  Serial.println(osajono);
  // Pieni viive ennen seuraavan loopin alkua
  delay(1000);
}
```

### Sample Output:
```
Moikka
```

## Deep Dive:
Substring-menetelmät ovat olleet ohjelmointimaailmassa pitkään, tarjoten keinoja käsitellä merkkijonoja joustavasti. Arduinossa `String`-luokan `substring()` funktio on se tyypillisin tapa. Vaihtoehtoisesti voi käyttää C:n tyylistä merkkijonon käsittelyä char-taulukoiden ja funktioiden, kuten `strncpy()`, avulla, mutta tämä voi olla monimutkaisempaa ja virhealtista.

Tehokkuus mielessä, `substring()` luo uuden `String`-olion, mikä saattaa ajan myötä johtaa muistiongelmiin, erityisesti pienillä laitteilla kuten Arduinolla. Tämä fragmentaation riski on hyvä pitää mielessä ja valvoa `String`-objektien käyttöä.

## See Also:
- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/
- Alternatives to String: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Memory Management: https://www.arduino.cc/en/Tutorial/Memory
