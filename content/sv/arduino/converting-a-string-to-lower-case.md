---
title:    "Arduino: Omvandla en sträng till små bokstäver"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera en sträng till små bokstäver är en viktig funktion i många Arduino-projekt, särskilt om man arbetar med användarinput eller data från sensorer. Det kan också vara användbart om man vill jämföra strängar eller skriva ut text på en display.

## Hur man gör
För att konvertera en sträng till små bokstäver i Arduino, används funktionen `toLowerCase()`. Det är viktigt att notera att denna funktion endast fungerar med ASCII-tecken och inte med utökade/utländska tecken.

```Arduino
String str = "Hej världen";
String lowerStr = str.toLowerCase();
Serial.println(lowerStr); // output: hej världen
```

Om man vill undvika att skapa en ny sträng varje gång, kan man istället använda `.toLowerCase()` på en redan existerande sträng.

```Arduino
String str = "Hej världen";
str.toLowerCase();
Serial.println(str); // output: hej världen
```

Om man arbetar med tecken arrayer istället för strängar, kan man använda en for-loop och funktionen `tolower()` för att konvertera varje tecken till ett litet tecken. Här är ett exempel på hur man kan göra detta:

```Arduino
char str[] = "Hej världen";
for (int i = 0; str[i] != '\0'; i++) {
  str[i] = tolower(str[i]);
}
Serial.println(str); // output: hej världen
```

## Djupdykning
När man konverterar en sträng till små bokstäver genom att använda `toLowerCase()` funktionen, ändras endast bokstäverna i strängen. Andra tecken, som siffror och specialtecken, påverkas inte. Detta är viktigt att ha i åtanke när man jämför strängar.

Dessutom måste man välja rätt teckenkodning för att kunna konvertera strängar till små bokstäver. Om man exempelvis arbetar med en Unicode-teckenkodning, behöver man en annan funktion för att konvertera strängar till små bokstäver.

## Se även
* [referens för `toLowerCase()` funktionen](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
* [ASCII-teckenkodning](https://www.ascii-code.com/)