---
date: 2024-01-20 17:34:09.893713-07:00
description: "Slik gj\xF8r du: I Arduinos barndom, pre-2000-tallet, var strengmanipulering\
  \ krevende grunnet begrenset minne og prosesseringskraft. Programmerere brukte\u2026"
lastmod: '2024-04-05T21:53:42.012340-06:00'
model: gpt-4-1106-preview
summary: I Arduinos barndom, pre-2000-tallet, var strengmanipulering krevende grunnet
  begrenset minne og prosesseringskraft.
title: "Sammensl\xE5ing av strenger"
weight: 3
---

## Slik gjør du:
```Arduino
void setup() {
  Serial.begin(9600);

  String hilsen = "Hei";
  String navn = "Olav";
  String melding = hilsen + ", " + navn + "!";

  Serial.println(melding); // Skriver ut: Hei, Olav!
}
void loop() {
  // Koden vår krever ikke noe i loop-funksjonen
}
```

## Dypdykk
I Arduinos barndom, pre-2000-tallet, var strengmanipulering krevende grunnet begrenset minne og prosesseringskraft. Programmerere brukte ofte C-karakterarrayer og lavnivå-funksjoner som `strcat()`. Men, med introduksjonen av `String`-klassen i Arduino, ble konkatenering mye enklere. 

Konkatenering kan også gjøres med `sprintf()`, som gir mer kontroll over formateringen, eller med `strcat()` for karakterarrayer for de som foretrekker C-stil. Det bør nevnes at overdreven bruk av `String`-objekter kan føre til fragmentering av minnet på grunn av måten de allokere og frigjør minne, så for styringssystemer som kjører kontinuerlig, kan alternative metoder være bedre.

## Se Også
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [C Plus Plus - Strings](http://www.cplusplus.com/reference/string/string/)
