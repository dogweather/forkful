---
title:    "Arduino: Erstellen einer temporären Datei"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Die Erstellung vorübergehender Dateien ist ein wichtiger Teil der Programmierung mit Arduino. Diese temporären Dateien werden verwendet, um Daten oder Variablen zwischenspeichern, die während der Ausführung eines Programms benötigt werden. Durch die Erstellung und Verwendung von temporären Dateien können wir effizienter und effektiver programmieren.

## Wie erstelle ich eine temporäre Datei mit Arduino?

Es gibt verschiedene Methoden, um eine temporäre Datei mit Arduino zu erstellen. Eine Möglichkeit ist die Verwendung der `File`-Bibliothek. Hier ist ein Beispielcode, der eine temporäre Datei mit dem Namen `temp_data.txt` erstellt und einige Daten in sie schreibt:

```Arduino
#include <File.h>

void setup() {

  // Eine Datei namens "temp_data.txt" im Schreibmodus erstellen
  File tempFile = SPIFFS.open("temp_data.txt", "w");

  // Daten in die Datei schreiben
  tempFile.print("Temperatur: ");
  tempFile.println(25);
  tempFile.print("Luftfeuchtigkeit: ");
  tempFile.println(50);

  // Datei schließen
  tempFile.close();

}

void loop() {
  // Programmablauf fortsetzen
}
```

Die oben genannte Methode ist nützlich, wenn Sie temporäre Dateien erstellen möchten, die nur während der Ausführung des Programms benötigt werden. Wenn Sie jedoch eine temporäre Datei erstellen möchten, die auch nach dem Ausschalten des Arduino gespeichert bleibt, können Sie die `EEPROM`-Bibliothek verwenden. Hier ist ein Beispielcode, der eine temporäre Datei namens `temp_data.txt` in der EEPROM erstellt:

```Arduino
#include <EEPROM.h>

void setup() {
  // Eine byte-Datenarray erstellen
  byte tempData[10];
  
  // Daten in das Array schreiben
  tempData[0] = 25;
  tempData[1] = 50;

  // Daten in der EEPROM speichern
  EEPROM.put(0, tempData);
}

void loop() {
  // Programmablauf fortsetzen
}
```

## Tiefer Einblick

Die oben genannten Methoden sind nur zwei Beispiele von vielen Möglichkeiten, um temporäre Dateien in Ihren Arduino-Projekten zu verwenden. Wenn Sie einen tieferen Einblick in die Erstellung von temporären Dateien mit Arduino erhalten möchten, können Sie sich mit den verschiedenen Bibliotheken wie `SPIFFS`, `SD` oder `EEPROM` vertraut machen. Diese Bibliotheken bieten viele Funktionen und Methoden, die Ihnen helfen, Ihre temporären Dateien effizienter zu nutzen.

## Siehe auch

- [Arduino-Referenz für die Dateiverwaltung](https://www.arduino.cc/en/Reference/File)
- [EEPROM-Bibliotheksdokumentation](https://www.arduino.cc/en/Reference/EEPROM)