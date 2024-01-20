---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:34:49.074757-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng innebär att göra om textformat till datumformat som programmet kan använda. Programmerare gör detta för att lättare hantera och manipulera datumdata, användbart för allt från att sätta tidsstämplar till att skapa schemaläggningssystem.

## How to:
```Arduino
#include <Wire.h>
#include <RTClib.h>

RTC_DS3231 rtc;

void setup() {
  Serial.begin(9600);
  if (!rtc.begin()) {
    Serial.println("Couldn't find RTC");
    while (1);
  }

  // Anta att vi har en datumsträng i formatet "DD-MM-YYYY"
  String dateString = "23-04-2023";

  // Parse the string to individual parts
  int day = dateString.substring(0, 2).toInt();
  int month = dateString.substring(3, 5).toInt();
  int year = dateString.substring(6).toInt();

  // Sätt RTC till det parsade datumet
  rtc.adjust(DateTime(year, month, day));

  // Visa det parsade datumet
  DateTime now = rtc.now();
  Serial.print(now.day());
  Serial.print('/');
  Serial.print(now.month());
  Serial.print('/');
  Serial.println(now.year());
}

void loop() {
  // Inget att göra här
}
```

## Deep Dive:
Att parsning av datum och tid har en lång historia i programmering är ingen nyhet. Före väletablerade bibliotek var programmerare tvungna att själva hantera alla aspekter av tolkningen, vilket ledde till många fel och inkompatibiliteter. 

I Arduino-världen finns bibliotek som RTClib som förenklar tolkning och hantering av datum och tid. Arduino självt har ingen inbyggd support för datum och tidsformat, det kräver tillsatshårdvara (som en RTC – Real Time Clock – modul) och bibliotek som RTClib.

Alternativ till RTClib kan inkludera TimeLib eller egna funktioner om det är en liten projektet. Viktigt att här komma ihåg är standarder för datumformat; internationellt är ISO 8601 (YYYY-MM-DD) standard, men i olika länder kan lokala format (som DD-MM-YYYY i Europa) också användas. Stränghantering är alltid kritisk, och man bör vara medveten om att formaten kan variera.

När det gäller implementation kräver datumtolkning från sträng hantering av textsträngar (strings), som kan innehålla fel och oavsiktliga tecken. Validering och felsökning är därför nyckelkomponenter i processen.

## Se Även:
- [RTClib GitHub-repository](https://github.com/adafruit/RTClib)
- [ISO 8601 standard information](https://www.iso.org/iso-8601-date-and-time-format.html)