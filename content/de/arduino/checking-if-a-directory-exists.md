---
title:                "Arduino: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung mit Arduino gibt es oft das Szenario, dass man überprüfen möchte, ob ein bestimmtes Verzeichnis vorhanden ist. Ein Beispiel für die Anwendung dieser Funktion ist, wenn man Daten aus einem Verzeichnis lesen oder schreiben möchte. In diesem Blogbeitrag werde ich Ihnen zeigen, wie Sie mit Arduino prüfen können, ob ein Verzeichnis vorhanden ist und was Sie dabei beachten sollten.

## Wie

Um zu überprüfen, ob ein Verzeichnis existiert, gibt es in der Arduino-Programmierung eine spezielle Funktion namens "exists()". Diese Funktion überprüft, ob der angegebene Pfad existiert und gibt eine "true" oder "false" Rückgabe zurück. Um diese Funktion zu verwenden, müssen Sie zuerst die SD-Library in Ihrem Sketch einbinden.

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600); // Serielle Verbindung zum PC herstellen
  if (SD.exists("/meinVerzeichnis")) { // Überprüfung, ob Verzeichnis existiert
    Serial.println("Das Verzeichnis existiert!");
  }
  else {
    Serial.println("Das Verzeichnis existiert nicht!");
  }
}

void loop() {
  
}
```

In diesem Beispiel wird geprüft, ob das Verzeichnis "meinVerzeichnis" vorhanden ist. Wenn dies der Fall ist, wird eine entsprechende Meldung auf dem seriellen Monitor ausgegeben. Ansonsten wird die Meldung angezeigt, dass das Verzeichnis nicht existiert.

## Deep Dive

Bei der Verwendung der "exists()" Funktion gibt es einige Dinge zu beachten. Zum einen müssen Sie sicherstellen, dass Sie die richtige Schreibweise des Verzeichnisnamens verwenden. Groß- und Kleinschreibung müssen dabei genau übereinstimmen. Andernfalls erkennt die Funktion das Verzeichnis nicht als vorhanden.

Außerdem ist es wichtig zu wissen, dass die "exists()" Funktion auch überprüft, ob eine Datei mit dem angegebenen Namen vorhanden ist. Wenn also bereits eine Datei mit dem Namen "meinVerzeichnis" existiert, wird die Funktion auch dann "true" zurückgeben, selbst wenn kein Verzeichnis mit diesem Namen vorhanden ist.

Darüber hinaus kann die "exists()" Funktion auch verwendet werden, um zu überprüfen, ob eine Datei vorhanden ist. Dazu wird einfach der vollständige Pfad der Datei als Parameter angegeben.

## Siehe auch

- [SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Tutorial: Arduino und SD-Karte](https://funduino.de/nr-31-tutorial-arduino-und-sd-karte)
- [Arduino Forum: How to check if the directory exist](https://forum.arduino.cc/index.php?topic=326169.0)