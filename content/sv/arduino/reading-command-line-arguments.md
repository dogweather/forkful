---
title:    "Arduino: Läsning av kommandoradsargument"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Varför du borde läsa kommandoradsargument

Att kunna läsa kommandoradsargument är en användbar förmåga när du arbetar med Arduino programmering. Det ger dig möjlighet att skicka och ta emot specifika instruktioner från din Arduino via datorn.

## Så här gör du

För att kunna läsa kommandoradsargument behöver du först och främst förstå syntaxen. Det finns tre huvudsakliga delar som är viktiga att känna till:

1. Det första argumentet är alltid programmets namn (i detta fall "Arduino").
2. Det andra argumentet är vilken port som används för kommunikation.
3. Eventuella efterföljande argument är de faktiska kommandona som önskas skickas till Arduino.

För att läsa kommandoradsargument i din Arduino kod använder du funktionen ```main()```. Här är ett enkelt exempel som skriver ut det andra argumentet (portskilt) på seriell monitor:

```
#include <stdio.h>

int main(int argc, char *argv[]) {
  Serial.begin(9600);
  
  Serial.println(argv[2]);

  while (1) { }
  return 0;
}
```

När du nu kör programmet via kommandoraden: ```Arduino COM5 Hello``` kommer "Hello" att skrivas ut på seriell monitor.

## Djupdykning

När du väl behärskar läsning av kommandoradsargument finns det många användbara saker du kan göra med det. Du kan till exempel använda det för fjärrstyrning av din Arduino eller för att skicka data från sensormoduler via kommandoraden. Det kan också underlätta felsökning genom att du kan testa kommandon utan att behöva rensa och ladda om din kod.

## Se även

- [Arduino Command Line Interface](https://www.arduino.cc/en/Guide/CLIDrivers)
- [Arduino Serial communication](https://www.arduino.cc/en/Reference/Serial)
- [Arduino command line arguments tutorial video](https://www.youtube.com/watch?v=u9evJiB-3Mg)