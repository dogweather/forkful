---
title:                "Vergleich von zwei Daten"
html_title:           "Arduino: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Vergleichung von zwei Datumsangaben ist eine häufig verwendete Funktion in der Programmierung. Programmierer nutzen sie, um festzustellen, welches Datum früher oder später ist oder um zu überprüfen, ob zwei Ereignisse am selben Tag stattfinden.

## Wie geht's?
Um zwei Datumsangaben in Arduino zu vergleichen, können Sie die Funktion ```if()``` verwenden. Sie müssen jedoch sicherstellen, dass die Datumsangaben in einem bestimmten Format vorliegen, zum Beispiel als String oder als Integer. Hier ist ein Beispielcode, der dies verdeutlicht:

```Arduino
int date1 = 20200506; // Datum 1 in dem Format JJJJMMTT
int date2 = 20191231; // Datum 2 in dem Format JJJJMMTT

if(date1 > date2) {
  Serial.println("Datum 1 ist später als Datum 2");
} else if(date1 < date2) {
  Serial.println("Datum 2 ist später als Datum 1");
} else {
  Serial.println("Datum 1 und Datum 2 sind gleich");
}
```

Das obige Beispiel zeigt, wie die Vergleichung von zwei Datumsangaben in Arduino funktioniert. Sie können auch mit anderen Arten von Datumsangaben experimentieren, wie zum Beispiel mit Strings. Beachten Sie jedoch, dass dabei möglicherweise zusätzlicher Code benötigt wird, um das Datum in das richtige Format zu konvertieren.

## Tief ins Detail
Die Verwendung von Funktionen zur Vergleichung von Daten ist eine grundlegende Programmierfähigkeit. Sie können jedoch auch speziellere Funktionen und Libraries in Arduino verwenden, die die Vergleichung von Datumsangaben erleichtern. Ein Beispiel dafür ist die Library "Time", die auf GitHub verfügbar ist. Diese Library stellt verschiedene Funktionen zur Verfügung, um das aktuelle Datum und die aktuelle Uhrzeit abzurufen und mit anderen Datumsangaben zu vergleichen.

## Siehe auch
Wenn Sie mehr über die Vergleichung von Datumsangaben und die Verwendung von Libraries in Arduino erfahren möchten, empfehlen wir Ihnen, die folgenden Quellen zu lesen:

- Arduino Reference: https://www.arduino.cc/reference/en/
- "Time" Library auf GitHub: https://github.com/PaulStoffregen/Time