---
title:    "Arduino: Eine Datumsangabe in einen String umwandeln."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Warum
Ein ständiger Begleiter bei der Programmierung ist die Umwandlung von Daten in verschiedene Formate. Eine häufige Aufgabe ist zum Beispiel das Konvertieren eines Datums in einen String. In diesem Blogpost erfahren Sie, wie Sie dies mit Arduino bewerkstelligen können.

## Wie geht das?
Um ein Datum in einen String umzuwandeln, müssen wir zuerst das Datum als Integer-Wert erhalten. Dazu können wir die Funktion `millis()` verwenden, welche die Anzahl an Millisekunden seit dem Start des Arduino-Boards zurückgibt. Anschließend können wir diese Zahl in eine Datumsstruktur umwandeln und diese dann in einen String konvertieren.

```Arduino
unsigned long now = millis(); // Millisekunden seit dem Start
String dateString = ""; // Leerer String, in den das Datum konvertiert wird
 
// Umwandlung in eine Datumsstruktur
tm *date = localtime(&now); 
 
// Konvertieren in einen String
dateString = String(date->tm_mday) + "." + String(date->tm_mon + 1) + "." + String(date->tm_year + 1900);
Serial.println(dateString); // Ausgabe: z.B. "15.09.2019"
```

## Tiefer gehend
Die Funktion `localtime()` wandelt die Anzahl an Sekunden seit dem 1. Januar 1970 in eine Datumsstruktur um. Dabei wird die Zeitzone automatisch berücksichtigt. Die umgewandelte Struktur beinhaltet alle Informationen, die für die Konvertierung in einen String benötigt werden. In unserem Beispiel nehmen wir das aktuelle Datum, aber Sie können auch ein beliebiges Datum in Millisekunden angeben und es in einen String konvertieren.

## Siehe auch
- [The Open Source Arduino Date Library](https://github.com/timreynolds/DateTime)
- [Umgang mit Zeit und Datum in der Arduino IDE](https://www.arduino.cc/en/Tutorial/BuiltInExamples/TimedAction)
- [Millisekunden seit Start des Arduino-Boards mit millis()](https://www.arduino.cc/reference/de/language/functions/time/millis/)