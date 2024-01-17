---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar är när man tar en del av en större text och använder den enskilda delen på ett separat sätt. Detta kan göras av programmerare för att effektivt hantera och manipulera data.

## Hur man gör:
Det är enkelt att extrahera substrängar i Arduino. Du behöver bara använda funktionen ```substring()``` och ange vilken del av texten du vill extrahera och på vilket sätt du vill använda den.

Exempel:
```
String text = "Hej! Det här är en text.";
String extraheradText = text.substring(4, 13); // Extraherar "Det här är"
```

Resultat:
```
extraheradText = "Det här är"
```

## Djupdykning:
Det finns flera olika sätt att extrahera substrängar på, men i Arduino är det vanligaste sättet att använda sig av funktionen ```substring()```.

Historiskt sett har substrängar varit användbara för att samla och hantera data, särskilt i tidiga datorprogram. I dag används det fortfarande flitigt i programmering för att hantera och manipulera textbaserade data.

Det finns också alternativ till att använda ```substring()```, såsom att använda andra metoder som ```indexOf()``` och ```split()```. Dessa kan också användas för att extrahera substrängar, men i vissa fall kan de vara mindre effektiva.

För implementeringsdetaljer om funktionen ```substring()```, kan du titta på Arduinos dokumentation eller läsa källkoden på GitHub.

## Se även:
- [Arduinos dokumentation om ```substring()```](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Källkoden för ```substring()``` på GitHub](https://github.com/arduino/ArduinoCore-avr/blob/master/cores/arduino/WString.cpp#L1515)
- [Andra funktioner för att manipulera text i Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)