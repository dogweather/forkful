---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:13.665323-07:00
description: "Hoe te: Om aanhalingstekens uit een string in Arduino te verwijderen,\
  \ kun je over de karakters lopen en de string herbouwen zonder de aanhalingstekens.\u2026"
lastmod: '2024-03-13T22:44:51.060676-06:00'
model: gpt-4-0125-preview
summary: Om aanhalingstekens uit een string in Arduino te verwijderen, kun je over
  de karakters lopen en de string herbouwen zonder de aanhalingstekens.
title: Quotes verwijderen uit een string
weight: 9
---

## Hoe te:
Om aanhalingstekens uit een string in Arduino te verwijderen, kun je over de karakters lopen en de string herbouwen zonder de aanhalingstekens. Bijvoorbeeld:

```arduino
String removeQuotes(String str) {
  String resultaat = ""; // Maak een lege string om het resultaat te houden
  for (int i = 0; i < str.length(); i++) {
    if (str[i] != '"' && str[i] != '\'') { // Controleer elk karakter
      resultaat += str[i]; // Voeg toe aan resultaat indien geen quote
    }
  }
  return resultaat;
}

void setup() {
  Serial.begin(9600);
  String testStr = "'Hallo, Wereld!'";
  Serial.println(removeQuotes(testStr)); // Zou moeten afdrukken: Hallo, Wereld!
}

void loop() {
  // Niets te doen hier
}
```

Voorbeelduitvoer op de Seriële Monitor zou zijn:
```
Hallo, Wereld!
```

## Diepgaande duik
Het concept van karakters uit een string verwijderen is niet uniek voor Arduino; het komt veel voor in veel programmeeromgevingen. Historisch gezien zijn stringmanipulatiefuncties een kernonderdeel van programmeertalen geweest om ontwikkelaars in staat te stellen gegevens effectief te reinigen en te analyseren.

Naast handmatig loopen en een nieuwe string opbouwen zoals hierboven getoond, zijn er alternatieve methoden. Men zou bijvoorbeeld de `replace()` methode kunnen gebruiken om aanhalingstekens te vervangen door een lege string, hoewel er afwegingen zijn qua leesbaarheid en het beheren van escape-karakters.

```arduino
String removeQuotes(String str) {
  str.replace("\"", ""); // Vervangt alle dubbele aanhalingstekens
  str.replace("\'", ""); // Vervangt alle enkele aanhalingstekens
  return str;
}
```

Het begrijpen van de afwegingen is cruciaal. De loopmethode kan langzamer zijn voor lange strings, maar is expliciet en gemakkelijk aan te passen (bijvoorbeeld als je alleen de leidende en sluitende aanhalingstekens wilt verwijderen). De `replace()` methode is beknopter en over het algemeen sneller, maar het wordt lastiger als er behoefte is om ontsnapte aanhalingstekens binnen de string te behandelen.

## Zie ook
- Arduino Stringreferentie: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- W3Schools' gids voor C++ stringmanipulatie (gerelateerd aan Arduino's taal): https://www.w3schools.com/cpp/cpp_strings.asp
- Stack Overflow discussies over stringmanipulatie in C++ (Arduino's basistaal): https://stackoverflow.com/questions/tagged/string+cpp
