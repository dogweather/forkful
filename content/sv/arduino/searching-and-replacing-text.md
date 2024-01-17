---
title:                "Sökning och ersättning av text"
html_title:           "Arduino: Sökning och ersättning av text"
simple_title:         "Sökning och ersättning av text"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Sökning och ersättning av text är en vanlig uppgift inom programmering. Det innebär att söka efter en viss text i en given sträng och ersätta den med en annan text. Programmarbetare gör detta för att kunna snabbt och effektivt byta ut eller ändra specifika delar av en kod, istället för att manuellt söka efter dem och ändra dem en efter en.

## Så här gör du:

För att söka och ersätta text i ditt Arduino-program, kan du använda funktionerna indexOf() och replace().

```Arduino
String myString = "Hej Svenska läsare!";
int index = myString.indexOf("läsare"); // söker efter första förekomsten av "läsare"
myString.replace(index, 7, "vänner"); // ersätter "läsare" med "vänner"
Serial.println(myString); // skriver ut: Hej Svenska vänner!
```

Du kan också använda funktionen replaceAll() för att söka och ersätta alla förekomster av en viss text i en sträng.

```Arduino
String myString = "Hej Sverige! Hej månsken! Hej finska sjöar!";
myString.replaceAll("Hej", "Hallå");
Serial.println(myString); // skriver ut: Hallå Sverige! Hallå månsken! Hallå finska sjöar!
```

## Djupdykning:

Sökning och ersättning av text är en viktig del av textmanipulering inom programmering. Det finns olika sätt att genomföra detta, såsom användning av reguljära uttryck eller bibliotek som tillhandahåller mer avancerade funktioner.

Vid implementering av sökning och ersättning av text är det viktigt att ta hänsyn till teckensättning och stor- och småbokstäver, eftersom dessa kan påverka sökresultaten.

## Se även:

Du kan läsa mer om funktionerna indexOf(), replace() och replaceAll() i Arduinos referensdokumentation:
[https://www.arduino.cc/reference/en/](https://www.arduino.cc/reference/en/)

För mer information om reguljära uttryck, kan du kolla in detta onlineverktyg som kan hjälpa dig skapa och testa dina uttryck:
[https://regex101.com/](https://regex101.com/)

Det finns också flera bibliotek som erbjuder utökade funktioner för sökning och ersättning av text, som till exempel det populära biblioteket "string-search" för Arduino:
[https://github.com/arduino-libraries/StringSearch](https://github.com/arduino-libraries/StringSearch)