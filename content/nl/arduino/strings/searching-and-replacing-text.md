---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:04.294119-07:00
description: "Tekst zoeken en vervangen stelt je in staat om specifieke tekens of\
  \ reeksen te vinden in een tekst en deze te vervangen door iets anders. Programmeurs\u2026"
lastmod: '2024-02-25T18:49:48.391382-07:00'
model: gpt-4-0125-preview
summary: "Tekst zoeken en vervangen stelt je in staat om specifieke tekens of reeksen\
  \ te vinden in een tekst en deze te vervangen door iets anders. Programmeurs\u2026"
title: Tekst zoeken en vervangen
---

{{< edit_this_page >}}

## Wat & Waarom?

Tekst zoeken en vervangen stelt je in staat om specifieke tekens of reeksen te vinden in een tekst en deze te vervangen door iets anders. Programmeurs doen dit om code, data of gebruikersinvoer moeiteloos te wijzigen.

## Hoe:

Arduino ondersteunt niet standaard het zoeken en vervangen van strings zoals hogere programmeertalen dat doen. Je kunt echter werken met tekenarrays of de `String`-klasse gebruiken, die de `replace()`-methode biedt. Hoewel de eerste optie geheugen-efficiënt is, is de laatste meer rechttoe rechtaan. Laten we ons concentreren op de `String`-klasse voor de duidelijkheid.

```Arduino
void setup() {
  Serial.begin(9600);
  String tekst = "Ik vind appels leuk en appels zijn geweldig!";
  tekst.replace("appels", "sinaasappels");
  Serial.println(tekst);
}

void loop() {
  // Niets te doen hier.
}
```

Voorbeelduitvoer:
```
Ik vind sinaasappels leuk en sinaasappels zijn geweldig!
```

## Diepere Duik

Vroeger waren stringmanipulatietaken op microcontrollers zeldzaam — geheugen was beperkt en toepassingen waren eenvoudiger. Tegenwoordig, met complexere projecten en voldoende geheugenruimte (dankzij de vooruitgang in microcontrollertechnologie), zijn dergelijke hulpprogramma's vrij standaard.

Als je de `String`-klasse niet wilt gebruiken vanwege het dynamische geheugengebruik, dat fragmentatie kan veroorzaken, kun je nog steeds zoeken en vervangen in C-stijl strings (null-geëindigde tekenarrays) met functies zoals `strchr()`, `strstr()`, en handmatige kopieën of vervangingen met lussen. Het is ingewikkelder maar geeft je meer controle over het geheugen.

Bijvoorbeeld, een alternatieve manier om een deelstring te vervangen zou er zo uit kunnen zien:

```Arduino
void replaceSubstring(char *input, const char *search, const char *replace) {
  char buffer[100];
  char *p;

  // 'strstr' controleert of 'search' deel uitmaakt van 'input'.
  if (!(p = strstr(input, search))) return;

  // Kopieer tot het punt waar 'search' wordt gevonden.
  strncpy(buffer, input, p - input);
  buffer[p - input] = '\0';

  // Voeg 'replace' toe en de rest van 'input' na 'search'.
  sprintf(buffer+(p - input), "%s%s", replace, p + strlen(search));

  // Resultaat uitvoeren
  strcpy(input, buffer);
}

void setup() {
  Serial.begin(9600);
  char tekst[] = "Ik vind appels leuk en appels zijn geweldig!";
  replaceSubstring(tekst, "appels", "sinaasappels");
  Serial.println(tekst);
}

void loop() {
  // Nog steeds niets te doen hier.
}
```

Voorbeelduitvoer:
```
Ik vind sinaasappels leuk en sinaasappels zijn geweldig!
```

## Zie Ook

- [Arduino Referentie: String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino Referentie: String Vervang Functie](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [Cplusplus.com: C Stringfuncties](http://www.cplusplus.com/reference/cstring/)
