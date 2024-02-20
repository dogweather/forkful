---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:15.574406-07:00
description: "De huidige datum op een Arduino krijgen betekent het bevragen van een\
  \ realtimeklok (RTC) of een internetgebaseerde tijdsdienst om de datum van dit moment\u2026"
lastmod: 2024-02-19 22:05:10.167092
model: gpt-4-0125-preview
summary: "De huidige datum op een Arduino krijgen betekent het bevragen van een realtimeklok\
  \ (RTC) of een internetgebaseerde tijdsdienst om de datum van dit moment\u2026"
title: Het huidige datum ophalen
---

{{< edit_this_page >}}

## Wat & Waarom?
De huidige datum op een Arduino krijgen betekent het bevragen van een realtimeklok (RTC) of een internetgebaseerde tijdsdienst om de datum van dit moment te achterhalen. Waarom dit doen? Gebeurtenissen loggen, data van een tijdstempel voorzien, of acties plannen—het kennen van de datum kan cruciaal zijn voor deze taken.

## Hoe:

Laten we onze Arduino slim maken over de datum. We gebruiken een RTC-module, zoals de DS3231, die nauwkeurig is en een backupbatterij heeft.

```arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);

  if (!rtc.begin()) {
    Serial.println("Kon RTC niet vinden");
    while (1);
  }

  if (rtc.lostPower()) {
    Serial.println("RTC heeft stroom verloren, laten we de tijd instellen!");
    // volgende regel stelt de RTC in op de datum & tijd waarop deze sketch is gecompileerd
    rtc.adjust(DateTime(F(__DATE__), F(__TIME__)));
  }
}

void loop() {
  DateTime now = rtc.now();

  Serial.print(now.year(), DEC);
  Serial.print('/');
  Serial.print(now.month(), DEC);
  Serial.print('/');
  Serial.print(now.day(), DEC);
  
  delay(3000); // wacht 3 seconden voordat de datum wordt bijgewerkt
}
```

Voorbeelduitvoer:
```
2023/4/5
```

## Diepere Duik:
Historische context? Vroege computers hadden de datum niet nodig. Het werd pas belangrijk toen we begonnen met loggen en multi-gebruikerssystemen. Tegenwoordig wordt het gewoon verwacht.

Alternatieven voor RTC's bevatten het gebruik van het Network Time Protocol (NTP) wanneer verbonden met het internet, of GPS-modules die nauwkeurige tijd- en datuminformatie bieden.

Implementatiedetails doen ertoe. Niet alle RTC's zijn gelijk geschapen. Sommige, zoals de DS1307, zijn minder nauwkeurig en kunnen meer over tijd afdwalen. Bibliotheken zoals `RTClib.h` abstraheren de verschillen tussen modules, wat je leven makkelijker maakt.

NTP over WiFi gebruiken vereist een andere benadering. Je hebt een ESP8266 of ESP32 met internettoegang nodig, en moet bibliotheken zoals `WiFi.h` en `NTPClient.h` includeren. Het codeerpatroon verandert— je maakt periodieke verzoeken aan een tijdserver en analyseert de resultaten voor de datum.

## Zie Ook:
- [RTClib bibliotheek](https://github.com/adafruit/RTClib): Een bibliotheek die interface met RTC's een briesje maakt.
- [DS3231 datasheet](https://datasheets.maximintegrated.com/en/ds/DS3231.pdf): De details over de DS3231 RTC-module.
- [NTPClient bibliotheek](https://github.com/arduino-libraries/NTPClient): Voor het verkrijgen van tijd via het internet.
- [Tijd en Datum op Arduino Zonder een RTC](https://create.arduino.cc/projecthub/Arnov_Sharma_makes/time-and-date-on-arduino-without-a-rtc-module-c7d2d6): Alternatieve methoden als je geen RTC gebruikt.
