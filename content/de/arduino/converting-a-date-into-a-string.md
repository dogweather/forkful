---
title:                "Eine Datumangabe in einen String umwandeln"
html_title:           "Arduino: Eine Datumangabe in einen String umwandeln"
simple_title:         "Eine Datumangabe in einen String umwandeln"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Du fragst dich vielleicht, warum du überhaupt ein Datum in einen String umwandeln solltest. Nun, in der Welt der Arduino Programmierung gibt es viele Anwendungen für die Ausgabe von Daten in Form eines Strings. Zum Beispiel können wir so Daten auf einem LCD-Display anzeigen oder sie drahtlos über Bluetooth übertragen.

## Wie geht's

Das Umwandeln eines Datums in einen String mag zunächst etwas verwirrend erscheinen, aber keine Sorge, es ist nicht so kompliziert, wie es klingt. Mit der folgenden Arduino Code-Schnipsel können wir das aktuelle Datum in einen String umwandeln und ihn auf dem Seriellen Monitor ausgeben.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // warte auf die Verbindung
  }
  time_t now = time(0);
  char buffer[26];
  struct tm *tm_struct = localtime(&now);
  strftime(buffer, 26, "%Y-%m-%d %H:%M:%S", tm_struct);
  Serial.println(buffer);
}

void loop() {
  // tu nichts
}
```

Die Ausgabe des obigen Codes sollte wie folgt aussehen:

```
2019-09-29 12:30:45
```

Wie du sehen kannst, haben wir das aktuelle Datum in einen String umgewandelt und konnten es so auf dem Seriellen Monitor ausgeben. Probier es doch mal selbst aus und schau, ob du es schaffst, den String in eine andere Formatierung umzuwandeln. 

## Tiefenschürfung

Um ein Datum in einen String umzuwandeln, müssen wir zunächst eine Variable vom Typ `time_t` erstellen, die die aktuelle Zeit enthält. In unserem Beispiel haben wir die Zeitbibliothek `TimeLib.h` verwendet, die es uns ermöglicht, auf die aktuelle Zeit zuzugreifen. Anschließend erstellen wir ein char-Array `buffer`, in dem der String gespeichert wird. Dann nutzen wir `localtime()` zusammen mit `tm_struct`, um die Zeit in eine strukturierte Form umzuwandeln. Schließlich verwenden wir `strftime`, um den String in der von uns gewünschten Formatierung zu erstellen. Du kannst verschiedene Formate ausprobieren, um das Datum in verschiedenen Stilen auszugeben.

## Siehe auch

- [Umgang mit Zeit und Datum auf dem Arduino](https://www.arduino.cc/reference/de/language/functions/time/)
- [Zeitbibliothek Dokumentation](https://playground.arduino.cc/code/time/)
- [Arduino - String in char-Array konvertieren](https://www.novemberfive.co/blog/convert-string-to-char-array-arduino)