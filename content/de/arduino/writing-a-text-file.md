---
title:                "Arduino: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum?

Es gibt viele Gründe, warum man beim Programmieren eine Textdatei schreiben möchte. Möglicherweise möchtest du eine einfache Methode haben, um Daten zu speichern und später wieder abzurufen. Oder du möchtest Informationen an eine externe Komponente senden. Schreibst du eine Sensor-Datenaufzeichnung, möchtest du wahrscheinlich die Ergebnisse in einer Textdatei speichern, um sie später zu analysieren. Egal aus welchem Grund, das Schreiben von Textdateien kann eine nützliche Fähigkeit sein, die du bei deinen Arduino-Projekten verwenden kannst.

## Wie geht's?

Um eine Textdatei auf deinem Arduino zu schreiben, gibt es ein paar Schritte, die du befolgen musst:

1. Als Erstes musst du die SD-Bibliothek (SD-Library) in deinen Code einbinden.
2. Dann musst du eine SD-Karte in dein Arduino-Board einlegen und initialisieren.
3. Daraufhin kannst du eine Textdatei auf der SD-Karte erstellen und öffnen. Stelle sicher, dass du einen eindeutigen Namen für die Datei verwendest, z.B. `sensorDaten.txt`.
4. Verwende die `print()`- oder `write()`-Funktion, um Daten in die Datei zu schreiben.
5. Nachdem du alle Daten gespeichert hast, schließe die Datei und trenne die SD-Karte von deinem Arduino.

Hier ist ein Beispielcode, der eine Textdatei mit dem Namen `test.txt` erstellt und den Text "Hallo, Welt!" darin speichert:

```Arduino
#include <SD.h>

void setup() {
  Serial.begin(9600);
  
  // Verbinde mit der SD-Karte
  if (!SD.begin(4)) {
    Serial.println("Fehler beim Lesen der SD-Karte!");
    return;
  }

  // Öffne eine Datei mit dem Namen "test.txt"
  File file = SD.open("test.txt", FILE_WRITE);

  // Überprüfe, ob die Datei erfolgreich geöffnet wurde
  if (!file) {
    Serial.println("Fehler beim Öffnen der Datei!");
    return;
  }

  // Schreibe den Text "Hallo, Welt!" in die Datei
  file.write("Hallo, Welt!");
  
  // Schließe die Datei
  file.close();
  
  // Trenne die SD-Karte von deinem Arduino
  SD.end();
}

void loop() {
  
}
```

Sobald du den Code hochgeladen hast, kannst du die SD-Karte in deinem Computer öffnen und die Datei `test.txt` sehen. In dieser Datei sollte der Text "Hallo, Welt!" gespeichert sein.

## Tiefer gehender

Das Schreiben einer Textdatei auf deinem Arduino kann noch viele weitere Möglichkeiten bieten. Zum Beispiel kannst du Daten von Sensoren sammeln und alle paar Sekunden in einer Datei speichern, um ein detailliertes Protokoll deiner Messungen zu erstellen. Du könntest auch eine externe Komponente über die SD-Karte mit Informationen versorgen, indem du eine Textdatei mit Befehlen oder Nachrichten erstellst.

Eine wichtige Sache, die du beachten musst, ist die Begrenzung der Größe deiner Textdatei. Die meisten SD-Karten haben eine maximale Dateigröße von 4 GB, aber die tatsächliche Größe hängt von der Formatierung deiner Karte ab. Zusätzlich kann es je nach Arduino-Board und SD-Karte unterschiedliche Einschränkungen geben. Hast du Probleme beim Lesen und Schreiben von Textdateien, solltest du sicherstellen, dass deine Dateien die richtige Größe haben und deine SD-Karte mit deinem Arduino kompatibel ist.

## Siehe auch

- [SD-Library Referenz](https://www.arduino.cc/en/Reference/SD)
- [Arduino Tutorials - SD Card Module](https://www.arduino.cc/en/Tutorial/Files)
- [SparkFun Tutorial - Using the Arduino Pro Mini 3.3V](https://learn.sparkfun.com/tutorials/using-the-arduino-pro-mini-33v/all)