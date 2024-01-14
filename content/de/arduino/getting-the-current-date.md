---
title:                "Arduino: Das aktuelle Datum erhalten"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum
Warum sollte man sich überhaupt damit beschäftigen, das aktuelle Datum mit einem Arduino zu programmieren? Nun, es kann viele Gründe geben. Zum Beispiel kann es für die Entwicklung von Zeit- und Datums-sensitiven Projekten sehr nützlich sein, oder auch einfach nur um eine Uhr oder Kalenderfunktion in ein Projekt einzubinden.

# Wie geht's
Um das aktuelle Datum mit einem Arduino zu bekommen, musst du zunächst sicherstellen, dass du ein Real-Time-Clock (RTC) Modul an deinem Arduino angeschlossen hast. Dieses Modul wird verwendet, um genaue Zeit- und Datumsinformationen bereitzustellen. Es gibt verschiedene Arten von RTC-Modulen, aber in diesem Beispiel werden wir uns auf das DS3231 Modul konzentrieren.

Nachdem du dein Modul an den Arduino angeschlossen hast, musst du die erforderlichen Bibliotheken in deinem Sketch einbinden. Verwende dafür folgenden Codeblock:

```Arduino
#include <Wire.h>
#include <DS3231.h>
```

Als nächstes musst du in der `setup()` Funktion eine Verbindung zum RTC-Modul herstellen und das Datum initialisieren. Hier ist ein Beispielcode:

```Arduino
DS3231 rtc;
rtc.begin(); // Initialisierung des RTC-Moduls
DateTime now = rtc.now(); // Abrufen des aktuellen Datums
```

Jetzt hast du das aktuelle Datum im `now`-Objekt gespeichert und kannst es verwenden, um das Datum in der Seriellen Monitor-Schnittstelle auszugeben:

```Arduino
Serial.println(now.day()); // Tag im Monat (1-31)
Serial.println(now.month()); // Monat im Jahr (1-12)
Serial.println(now.year()); // Jahr (z.B. 2021)
```

# Tiefenschärfe
Neben den oben genannten Beispielen gibt es noch viele weitere nützliche Funktionen, die das DS3231 Modul bietet. Zum Beispiel kannst du mit der `rtc.adjust(DateTime(DateTimeData));` Funktion das Datum direkt im Modul einstellen, ohne es jedes Mal in deinem Code zu ändern. Außerdem hat das Modul auch einen Alarm-Modus, der es dir ermöglicht, bestimmte Aktionen basierend auf dem Datum oder der Uhrzeit auszuführen.

Es gibt auch verschiedene andere RTC-Module wie das DS1307 oder das PCF8563, die ähnliche Funktionen bieten. Wenn du also eine andere Art von RTC-Modul verwendest, können sich die oben genannten Beispiele etwas unterscheiden.

# Siehe auch
- Offizielle Arduino RTC Bibliothek (https://www.arduino.cc/en/Reference/RTC)
- Informationen zum DS3231 Modul (https://www.instructables.com/Using-the-DS3231-I2C-Real-Time-Clock-Module/)
- Tutorial zur Verwendung von RTC-Modulen mit Arduino (https://circuitdigest.com/microcontroller-projects/programming-rtc-using-arduino-and-a-ds3231-rtc-module)