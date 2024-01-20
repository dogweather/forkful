---
title:                "Att göra en sträng versal"
html_title:           "Bash: Att göra en sträng versal"
simple_title:         "Att göra en sträng versal"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att sätta en sträng till versaler innebär att omvandla alla bokstäver i den till stora bokstäver. Programmerare gör detta för tydlighet, att framhäva text eller för att hantera textdata enhetligt.

## Hur man gör:
```Arduino
void setup() {
  Serial.begin(9600);
  String message = "Hej världen!";
  message.toUpperCase();
  Serial.println(message);
}

void loop() {
  // Tom loop
}
```
När du öppnar seriemonitorn kommer outputen att vara: `HEJ VÄRLDEN!`

## Fördjupning:
Det att göra om text till enbart versaler har rötter i tidiga datorsystem som bara stödde kapitalbokstäver. Alternativt kan strängmanipulation uppnås med funktioner såsom `toLowerCase()` för att få enbart små bokstäver eller `charAt()` i kombination med `toUpperCase()` för att göra om individuella tecken. När du använder `toUpperCase()` i Arduino, så är det metodens interna algoritm som går igenom varje tecken i strängen och omvandlar det till motsvarande versal om det finns en.

## Se Även:
- Arduino's `String` reference: [https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Unicode standarden för textrepresentation: [https://home.unicode.org/](https://home.unicode.org/)
- Praktiska exempel på strängmanipulering i C++ (språket bakom Arduino): [http://www.cplusplus.com/reference/string/string/](http://www.cplusplus.com/reference/string/string/)