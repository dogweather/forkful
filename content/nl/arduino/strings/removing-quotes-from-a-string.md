---
title:                "Quotes verwijderen uit een string"
date:                  2024-01-28T22:06:13.665323-07:00
model:                 gpt-4-0125-preview
simple_title:         "Quotes verwijderen uit een string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/arduino/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het verwijderen van quotes uit een string betekent het weglaten van enkele (`'`) of dubbele (`"`) aanhalingstekens die de tekst omgeven. Programmeurs doen dit vaak om invoer te saneren, strings voor te bereiden voor vergelijking, of tekstgegevens te verwerken die per ongeluk aanhalingstekens als deel van de stringinhoud kunnen bevatten.

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
