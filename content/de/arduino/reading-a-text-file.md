---
title:                "Ein Textdokument lesen"
html_title:           "Arduino: Ein Textdokument lesen"
simple_title:         "Ein Textdokument lesen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Das Lesen von Textdateien ist eine wichtige Fähigkeit für jeden, der mit Arduino arbeitet. Es ermöglicht uns, große Datenmengen zu verarbeiten und benutzerdefinierte Einstellungen oder Informationen abzurufen.

## Wie mache ich es
Das Lesen einer Textdatei auf einem Arduino ist eigentlich gar nicht so kompliziert. Wir müssen nur einige wichtige Schritte beachten:

1. Öffnen Sie die Datei mit der `file.open()` Funktion. Geben Sie den Dateinamen und den Modus (lesen, schreiben, anfügen) an.
2. Lesen Sie Zeilenweise mit der `file.readStringUntil()` Funktion. Diese liest die Daten bis zum nächsten Zeilenumbruch.
3. Verwenden Sie die `Serial.print()` Funktion, um die gelesenen Daten auf dem seriellen Monitor anzuzeigen.
4. Vergessen Sie nicht, die Datei mit der `file.close()` Funktion zu schließen, sobald Sie sie nicht mehr benötigen.

Hier ist ein Beispielcode, der eine Textdatei mit dem Namen "data.txt" liest und die Daten auf dem seriellen Monitor ausgibt:

```Arduino
File file = SD.open("data.txt", FILE_READ); // Öffnet die Datei im Lese-Modus

if (file) { // Überprüft, ob die Datei geöffnet wurde
  while (file.available()) { // Geht solange durch die Datei, bis das Ende erreicht ist
    String data = file.readStringUntil('\n'); // Liest die Daten bis zum nächsten Zeilenumbruch
    Serial.println(data); // Gibt die Daten auf dem seriellen Monitor aus
  }
  file.close(); // Schließt die Datei
}
```

Das wäre die Ausgabe auf dem seriellen Monitor:

```
Hallo, dies ist die erste Zeile!
Das ist die zweite Zeile.
Und hier ist die dritte Zeile.
```

## Tiefer schürfen
Es gibt einige Dinge, die wir beachten sollten, wenn wir Textdateien auf unserem Arduino lesen:

- Vergewissern Sie sich, dass Ihre Textdatei im richtigen Format ist. Sie sollte UTF-8 codiert sein, um sicherzustellen, dass Sonderzeichen richtig gelesen werden.
- Beachten Sie die maximalen Zeichen, die gleichzeitig von `readStringUntil()` gelesen werden können. Dies kann je nach verwendeter Arduino-Platine variieren.
- Wenn Sie spezielle Trennzeichen verwenden möchten, können Sie `readUntil()` anstelle von `readStringUntil()` verwenden und das Trennzeichen als Argument angeben.

## Siehe auch
- [SD-Library Referenz](https://www.arduino.cc/en/Reference/SD)
- [Serial.print() Funktion](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Arduino SD Karte verwenden](https://www.arduino.cc/en/Tutorial/ReadWrite)