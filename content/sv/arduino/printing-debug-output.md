---
title:    "Arduino: Utskrift av felsökningsutdata"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför
Debugging är en viktig del av programmering och kan göra skillnad mellan ett lyckat och misslyckat projekt. Genom att använda utskrift av felsökningsmeddelanden kan du enkelt spåra problem i ditt Arduino-program och effektivt lösa dem.

## Så här gör du
För att skriva ut felsökningsmeddelanden i ditt Arduino-program, behöver du först ansluta en seriell kommunikationsmodul till din Arduino. Därefter kan du använda funktionen "Serial.print" för att skriva ut värden och variabler på din dator via USB-porten.

```Arduino
// Anslut en seriell kommunikationsmodul till din Arduino
Serial.begin(9600);

// Skriv ut ett felsökningsmeddelande
Serial.print("Värdet av min variabel är: ");
Serial.println(minVariabel);
```

När du använder "Serial.print" funktionen, kan du också ange en radslutning genom att använda "println". Detta gör det lättare att läsa och organisera dina utskriftsmeddelanden.

## Djupdykning
Genom att utskriva felsökningsmeddelanden kan du enkelt jämföra dina förväntade värden med de faktiska värdena som visas på din dator. Detta är särskilt användbart när du arbetar med sensorer eller andra externa enheter.

Om du behöver utskriva mer avancerade meddelanden kan du också använda funktionen "Serial.write". Denna funktion kan skriva ut binära data och är särskilt användbar när du arbetar med dataöverföringar.

## Se även
- [Arduino Serial Communication](https://www.arduino.cc/en/Serial/)
- [Debugging Techniques for Arduino](https://blog.arduino.cc/2016/11/08/debugging-techniques-for-arduino/)
- [Serial.print vs Serial.write](https://arduino.stackexchange.com/questions/6211/serial-print-vs-serial-write)