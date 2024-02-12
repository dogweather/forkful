---
title:                "Extrahera delsträngar"
aliases:
- sv/arduino/extracting-substrings.md
date:                  2024-01-20T17:44:56.110167-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att extrahera substrängar innebär att ta specifika delar från en sträng av text. Programmerare gör detta för att bearbeta eller analysera data mer effektivt.

## How to: (Hur man gör:)
```Arduino
String text = "Hej, Arduino världen!";
String del = text.substring(5, 12); // Extrahera från index 5 till 11

Serial.begin(9600);
Serial.println(del);  // Skriver ut "Arduino"
```

Sample Output:
```
Arduino
```

Koden använder `substring()`-funktionen för att ta en deltext från den större strängen. 

## Deep Dive (Djupdykning)

Att extrahera substrängar har funnits med sedan tidiga programmeringsspråk. Det är en grundläggande operation som många program använder för att bearbeta text, som att hämta filnamn från sökvägar eller användarinfo från inloggning.

I Arduino C++, som är väldigt nära besläktat med C++, implementeras substrängsfunktionen genom `String`-klassen. Alternativ finns, såsom att använda C-funktioner som `strncpy()` eller pekararitmetik för att uppnå samma resultat utan att använda `String`-klassen, vilket kan vara bra för minneshantering.

`substring()`-funktionen som används i Arduino är lätt att använda men kan använda mer minne, vilket är en faktor att tänka på i minnesbegränsade miljöer som på mikrokontroller.

## See Also (Se även)
- Arduino `String` class reference: [https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- C++ substr function: [http://www.cplusplus.com/reference/string/string/substr/](http://www.cplusplus.com/reference/string/string/substr/)
- C string handling: [https://www.cprogramming.com/tutorial/c/lesson9.html](https://www.cprogramming.com/tutorial/c/lesson9.html)
