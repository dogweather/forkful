---
date: 2024-01-20 17:47:02.956104-07:00
description: "Hur g\xF6r man: F\xF6r Arduino finns funktionen `strlen()` f\xF6r C-stilens\
  \ str\xE4ngar (char arrays) och metoden `length()` f\xF6r String-objekt. H\xE4r\
  \ \xE4r exempel p\xE5 b\xE5da."
lastmod: '2024-03-13T22:44:38.159276-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r Arduino finns funktionen `strlen()` f\xF6r C-stilens str\xE4ngar\
  \ (char arrays) och metoden `length()` f\xF6r String-objekt."
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Hur gör man:
För Arduino finns funktionen `strlen()` för C-stilens strängar (char arrays) och metoden `length()` för String-objekt. Här är exempel på båda:

```arduino
void setup() {
  Serial.begin(9600); // Starta seriell kommunikation.
  
  // C-stil sträng (char array).
  char text1[] = "Hej Sverige!";
  Serial.println(strlen(text1));  // Skriv ut längden: 12

  // Arduino String-objekt.
  String text2 = "Arduino är kul!";
  Serial.println(text2.length()); // Skriv ut längden: 15
}

void loop() {
  // Här skulle din kod köra i en oändlig loop (inget för denna demonstration).
}
```
Kör koden, och titta i serieloggen för att se stränglängderna: 12 och 15.

## Djupdykning:
Historiskt sett är `strlen()` del av C:s standardbibliotek och räknar tecken tills den hittar en nollterminator. String-klassen i Arduino är senare tillkommen och `length()` fungerar oberoende av nollterminator. C-stilens `strlen()` kan vara snabbare men kräver att man hanterar minnet själv. `String.length()`, å andra sidan, är enklare att använda men kan leda till fragmentering av minnet om man inte är försiktig. Valet beror på situation och personlig preferens.

## Se även:
- Arduino's String reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ std::string documentation for a more robust string handling in advanced projects: http://www.cplusplus.com/reference/string/string/
- Memory management in Arduino if using String objects: https://www.arduino.cc/en/Tutorial/Memory
