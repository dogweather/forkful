---
date: 2024-01-26 00:58:53.400892-07:00
description: "\"Loggning\" inneb\xE4r att man f\xF6r en journal \xF6ver h\xE4ndelser,\
  \ transaktioner eller aktiviteter som sker \xF6ver tid i ett system. Programmerare\
  \ anv\xE4nder det f\xF6r\u2026"
lastmod: '2024-03-13T22:44:38.173624-06:00'
model: gpt-4-1106-preview
summary: "\"Loggning\" inneb\xE4r att man f\xF6r en journal \xF6ver h\xE4ndelser,\
  \ transaktioner eller aktiviteter som sker \xF6ver tid i ett system. Programmerare\
  \ anv\xE4nder det f\xF6r\u2026"
title: Loggning
---

{{< edit_this_page >}}

## Vad & Varför?
"Loggning" innebär att man för en journal över händelser, transaktioner eller aktiviteter som sker över tid i ett system. Programmerare använder det för att felsöka, övervaka systemhälsan, samla statistik eller till och med granska användningen, vilket gör det till en oumbärlig praxis för att underhålla och förstå beteendet hos deras kod under olika förhållanden.

## Hur gör man:
Arduino kommer inte med ett inbyggt loggningsbibliotek som vissa andra miljöer, men du kan implementera grundläggande loggning till Seriell-konsolen med minimal ansträngning. Här är ett snabbt exempel för att komma igång:

```arduino
void setup() {
  // Starta seriell kommunikation med den angivna baudhastigheten
  Serial.begin(9600);

  // Vänta på att seriellporten ansluter - bara nödvändigt på vissa kort
  while (!Serial) {
    ; // vänta på att seriellporten ska ansluta. Behövs för native USB
  }

  // Logga ett informativt meddelande som indikerar att installationsprocessen är klar
  Serial.println("Installationen klar!");
}

void loop() {
  // Enkel loggare som skriver ut drifttiden varje sekund
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Drifttid (ms): ");
    Serial.println(currentMillis);

    // Här kan du även lägga till felloggar, varningar eller annan info.
  }
  
  // Resten av din programlogik här...
}
```

Exempel på Seriell Utdata:
```
Installationen klar!
Drifttid (ms): 1000
Drifttid (ms): 2000
Drifttid (ms): 3000
...
```

## Fördjupning:
Historiskt sett var loggning på mikrokontrollers inte lika enkelt som på ett fullfjädrat operativsystem. Begränsade resurser innebar att varje byte räknades och utvecklare behövde vara noga med att inte tilltäppa systemet. Med ankomsten av mer kapabla kort och Arduino-plattformen som förenklade processen, har loggning blivit mer tillgänglig.

Medan koden ovan demonstrerar loggning via Seriellt interface, inkluderar andra metoder skrivning till ett SD-kort, att sända data över nätverk till en fjärrserver, eller till och med utskrift till en liten LCD-skärm.

Att implementera ett loggningssystem innebär att man måste ta hänsyn till saker som rotation, allvarlighetsnivå (info, debug, varning, fel) och påverkan på prestanda. På en Arduino kan du behöva vara medveten om minnesbegränsningar när du loggar komplexa datastrukturer. För fjärrloggning är även säkerheten för de överförda loggarna en oro.

Mer sofistikerade lösningar som Syslog, en bred accepterad loggningsstandard, existerar utanför Arduino-världen, men man kan integrera tredjepartsbibliotek som erbjuder liknande funktionalitet med olika grad av komplexitet och resurskrav.

## Se även:
- [Arduinos `Serial` referens](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [SD-kortloggning med Arduino](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFuns Data Logging sköld](https://www.sparkfun.com/products/13712)
- [TinyWeb: Ett praktiskt exempel på fjärrloggning med Arduino](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
