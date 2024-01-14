---
title:    "Arduino: Omvandla en sträng till gemener"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Varför

Att konvertera en sträng till gemener kan vara användbart i vissa situationer. Det kan till exempel underlätta vid jämförelse av strängar och förbättra användarupplevelsen i en applikation.

##Hur man gör

Att konvertera en sträng till gemener i Arduino är en relativt enkel process. Vi behöver bara använda en inbyggd funktion som heter “toLowerCase()”. Nedan visas ett kodexempel där vi tar en användarinput och konverterar den till gemener:

```Arduino
String input = "HEJ";
input.toLowerCase();  //Input blir nu "hej"
Serial.println(input); //Output: hej
```

Detta är det mest grundläggande exemplet, men det fungerar också med andra datatyper som char och byte.

##Djupdykning

När vi använder funktionen “toLowerCase()” så konverteras varje bokstav i strängen till sin gemena motsvarighet. Men vad händer om vi har en specialtecken eller ett åäö? Som standard kommer de inte att konverteras, utan de kommer fortfarande att vara stora bokstäver. För att lösa detta måste vi använda oss av en “map” som innehåller alla specialtecken och deras gemena motsvarigheter.

Vi kan definiera vår egen map genom att använda en array med tecken och sedan matcha dessa med motsvarande gemena bokstäver i en annan array. Här är ett exempel där vi lägger till åäö i vår map:

```Arduino
String input = "ÅÄÖ";
char specialChars[] = {'Å', 'Ä', 'Ö'};
char lowerChars[] = {'å', 'ä', 'ö'};
for (int i = 0; i < 3; i++) {
  input.replace(specialChars[i], lowerChars[i]);
}
Serial.println(input); //Output: åäö
```

Nu kommer vår sträng att konverteras på rätt sätt och output blir “åäö”. Vi kan också utöka vår map med fler speciella tecken om vi behöver det.

##Se även

Om du vill lära dig mer om strängmanipulering i Arduino så kan du titta på följande resurser:

- [String Functions in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)

- [Convert String to Lower Case in Arduino](https://arduino.stackexchange.com/questions/50/convert-string-to-lower-case)

- [Tutorial: String operations in Arduino](https://circuitdigest.com/microcontroller-projects/arduino-string-functions-examples)

Genom att behärska denna funktion kan du öppna upp nya möjligheter och förbättra dina Arduino projekt. Lycka till!