---
title:                "Arduino: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten kann in verschiedenen Anwendungen sehr nützlich sein, wie zum Beispiel in der Home-Automation, um zu prüfen, ob eine bestimmte Zeit überschritten wurde, oder in der Datenaufzeichnung, um zu sehen, ob Daten in einem bestimmten Zeitintervall aufgezeichnet wurden.

## Wie zu...

Die Arduino-Programmiersprache bietet verschiedene Funktionen, um das Vergleichen von zwei Daten einfach zu gestalten. Zunächst müssen die beiden Daten in einem geeigneten Datentyp definiert werden. In diesem Beispiel verwenden wir das `int` Datentyp für beide Daten. Dann kann der Vergleich mit dem `if`-Statement durchgeführt werden, um zu sehen, ob die erste Daten größer, kleiner oder gleich der zweiten Daten ist.

```Arduino
int date1 = 20201215;
int date2 = 20210101;

if (date1 > date2) {
  Serial.println("Date 1 is later than Date 2");
} else if (date1 < date2) {
  Serial.println("Date 1 is earlier than Date 2");
} else {
  Serial.println("Both dates are the same");
}
```

Die Ausgabe dieses Codes wäre "Date 1 is earlier than Date 2". Dies zeigt, dass das Programm in der Lage ist, die Datumsangaben richtig zu vergleichen.

## Tiefergehende Informationen

Die `if`-Bedingungen können auch mit anderen Vergleichsoperatoren wie größer-gleich (`>=`) oder kleiner-gleich (`<=`) kombiniert werden, um spezifischere Vergleiche zu machen. Es ist auch möglich, mehrere Daten miteinander zu vergleichen, indem man die `if`-Statements verschachtelt oder logische Operatoren wie `&&` (und) oder `||` (oder) verwendet.

Wenn es um das Vergleichen von Datum und Uhrzeit geht, kann es hilfreich sein, die Arduino Time Library zu verwenden, die spezielle Funktionen zum Parsen und Vergleichen von Datum und Uhrzeit bietet.

## Siehe auch

- https://www.arduino.cc/reference/de/language/structure/control-structure/if/
- https://www.arduino.cc/reference/de/language/variables/data-types/
- https://www.arduino.cc/reference/de/libraries/time/