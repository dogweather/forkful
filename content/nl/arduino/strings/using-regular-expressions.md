---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:33.004354-07:00
description: "Reguliere expressies (regex) laten je zoeken naar patronen in tekst\u2014\
  denk aan jokertekens op stero\xEFden. Programmeurs gebruiken ze om invoer te valideren,\u2026"
lastmod: '2024-03-13T22:44:51.062612-06:00'
model: gpt-4-0125-preview
summary: "Reguliere expressies (regex) laten je zoeken naar patronen in tekst\u2014\
  denk aan jokertekens op stero\xEFden."
title: Reguliere expressies gebruiken
weight: 11
---

## Hoe:
Arduino heeft geen ingebouwde regex-ondersteuning, maar je kunt eenvoudige patrooncontroles nabootsen. Voor geavanceerdere zaken kun je overwegen om een regex-bibliotheek zoals `Regexp` te gebruiken.

```Arduino
#include <Regexp.h>

void setup() {
  Serial.begin(9600);
  
  MatchState ms;
  char result;
  
  ms.Target ("Hello World!");
  result = ms.Match ("(World)");

  if (result > 0) {
    char captured[10]; // Zorg dat dit groot genoeg is om je match te bevatten
    ms.GetCapture (captured, 0);
    Serial.print("Match gevonden: ");
    Serial.println(captured);
  } else {
    Serial.println("Geen match gevonden.");
  }
}

void loop() {
  // Niets te doen hier.
}
```

Voorbeelduitvoer:
```
Match gevonden: World
```

## Diepgaande Duik
Regex is afkomstig uit de theoretische informatica en bestaat al sinds de jaren 50. Perl en andere talen hebben een sterke regex-implementatie, maar op Arduino zijn de middelen beperkt, dus geen native ondersteuning. Bibliotheken zoals `Regexp` zijn je vriend—ze nemen een deel van de last op zich, maar onthoud dat ze belastend kunnen zijn voor kleinere microcontrollers.

## Zie Ook
Check deze voor meer details:

- Arduino `Regexp` bibliotheek: [https://www.arduino.cc/reference/en/libraries/regexp/](https://www.arduino.cc/reference/en/libraries/regexp/)
- `Regexp` bibliotheek GitHub repo: [https://github.com/nickgammon/Regexp](https://github.com/nickgammon/Regexp)
- Online regex tester (voor het uitwerken van je regex voordat je deze implementeert): [https://regexr.com/](https://regexr.com/)
