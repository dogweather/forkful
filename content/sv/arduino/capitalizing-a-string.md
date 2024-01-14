---
title:                "Arduino: Att stora en sträng"
simple_title:         "Att stora en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att använda sig av en Arduino för att programmera kan verka som ett komplicerat och överväldigande projekt, men det kan faktiskt vara riktigt roligt och enkelt att lära sig. Ett av de enklaste projekten som man kan börja med är att lära sig hur man kapslar in en sträng med hjälp av en Arduino.

## Så här gör du

Först måste vi inkludera Arduinos <string.h> bibliotek för att kunna använda oss av funktionen för att kapsla in en sträng. Vi definierar sedan en ny variabel, låt oss kalla den "text", och tilldelar det en tidigare deklarerad sträng.

````Arduino
#include <string.h> // inkluderar <string.h>

void setup() {
  String text = "hej!"; // definierar och tilldelar en sträng, i detta fall "hej!"
  Serial.begin(9600); // startar seriell kommunikation på hastigheten 9600
  Serial.println(text.capitalize()); // anropar funktionen capitalize på textvariabeln och skriver ut det i seriell monitor
}

void loop() {
  // inget görs här eftersom vi bara vill köra denna kod en gång i "setup"-funktionen
}
````

Output i seriell monitor:

```
Hej!
```

Som du kan se har strängen "hej!" nu blivit kapslad i versaler. Detta kan vara användbart när man vill se till att en sträng är konsekvent formaterad oavsett vilka bokstäver som matas in.

## Djupdykning

För att förstå hur funktionen capitalize fungerar behöver vi titta närmare på koden i <string.h> biblioteket. Där kan vi hitta den här funktionen, som använder sig av en "for loop" och "toupper" funktionen för att omvandla varje enskild bokstav till dess lämpliga versal. Funktionen tar även hänsyn till specialtecken som t ex å, ä och ö.

````Arduino
String String::capitalize()
{
  String retval = *this;
  for (uint16_t i = 0; i < retval.length(); i++) {
    if ((i == 0) || (retval[i - 1] == '.') || (retval[i - 1] == '!') || (retval[i - 1] == '?')) {
      retval[i] = toupper(retval[i]);
    }
  }
  return retval;
}
````

Det är alltså en ganska enkel, men användbar funktion för att kapsla in en sträng i versaler.

## Se även

- <https://www.arduino.cc/reference/en/language/variables/data-types/string/>
- <https://www.geeksforgeeks.org/capitalizing-first-letter-of-a-string-in-javascript/>
- <https://docs.python.org/3/library/stdtypes.html#str.capitalize>