---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument i Arduino är processen där man tar in data vid körningstid för att justera programmets beteende. Detta används av programmerare för att göra programmen mer flexibla och anpassningsbara till olika miljöer och behov.

## Så här gör du:
Här är ett exempel på hur man överför kommandoradsargument till ett Arduino-program med `Serial` biblioteket.

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  String cmd;
  
  if (Serial.available()) {
    cmd = Serial.readStringUntil('\n');
    Serial.println("Command Received: " + cmd);
  }

  delay(500);
}
```

När du utför detta program och skickar en sträng via serieporten, kommer programmet att registrera strängen som ett argument och skriva ut det. 

## Djupdykning
Att kunna ta emot kommandoradsargument var mer relevant inom gammal skolans textbaserade programmeringsparadigmer där interaktionen var inriktad på kommandoraden. Inom modern programmering, såsom Arduino, är det inte lika vanligt, men principen kvarstår som en användbar teknik för att öka flexibiliteten för ditt program. 

Alternativet till direkt läsning via serieporten kan vara att inkludera dem i koden som konstanter, men detta tar bort möjligheten att ändra dessa värden utan att ändra själva koden.

Att läsa kommandoradsargument på detta sätt fungerar genom att programmet lyssnar på serieporten, väntar på inkommande data. När data kommer in, avslutas det av en nylinje ('n'). Denna rad av data skrivs in i en sträng och rätt kod kan använda den som önskat.

## Se också
- [Arduino Serial Documentation](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Understanding Command Line Arguments](https://www.cs.bu.edu/teaching/cpp/command-line-arguments/)
- [Arduino Programming Tutorial](https://startingelectronics.org/software/arduino/learn-to-program-course/)