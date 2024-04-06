---
date: 2024-01-20 17:50:16.369718-07:00
description: "Hur G\xF6r Man: Exempel p\xE5 utskrift: `Temp: 24.5C`."
lastmod: '2024-04-05T21:53:39.491703-06:00'
model: gpt-4-1106-preview
summary: "Exempel p\xE5 utskrift."
title: "Interpolera en str\xE4ng"
weight: 8
---

## Hur Gör Man:
```Arduino
char temperature[8];
int tempSensorValue = analogRead(A0);
float voltage = tempSensorValue * 5.0;
voltage /= 1024.0;
float temperatureC = (voltage - 0.5) * 100.0;
sprintf(temperature, "Temp: %fC", temperatureC);
Serial.println(temperature);
```
Exempel på utskrift: `Temp: 24.5C`

## Djupdykning:
Interpolering i Arduino använder `sprintf()`, som kommer från C-språket. Alternativ inkluderar att sammanfoga strängar med `strcat()` eller använda `String`-klassen med dess `+`-operator. Men `sprintf()` är snabbt och sparsamt med minne – perfekt för små mikrokontroller som Arduino.

## Se Även:
- Arduino's String reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ `std::sprintf`: http://www.cplusplus.com/reference/cstdio/sprintf/
- Arduino analogRead: https://www.arduino.cc/reference/en/language/functions/analog-io/analogread/
