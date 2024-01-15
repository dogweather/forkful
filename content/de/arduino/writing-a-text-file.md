---
title:                "Eine Textdatei schreiben"
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben eines Textdatei auf einem Arduino kann hilfreich sein, um Daten oder Statusmeldungen zu speichern. Dies kann besonders nützlich sein, wenn der Arduino über längere Zeit unabhängig arbeitet oder eine Datenprotokollierung benötigt wird.

## Wie man eine Textdatei auf einem Arduino schreibt

Um eine Textdatei auf einem Arduino zu schreiben, können wir die eingebaute SD-Bibliothek verwenden. Zunächst müssen Sie die SD-Karte an Ihren Arduino anschließen und die Bibliothek in Ihrem Sketch einbinden:

```Arduino
#include <SD.h>
```

Als nächstes müssen wir die SD-Karte initialisieren und öffnen:

```Arduino
if (!SD.begin()) {
  Serial.println("Fehler beim Initialisieren der SD-Karte!");
  return;
}

File myFile = SD.open("meineDatei.txt", FILE_WRITE);
```

Jetzt können wir Daten in die Textdatei schreiben:

```Arduino
myFile.println("Dies ist ein Beispieltext.");
```

Und zum Schluss müssen wir die Datei schließen:

```Arduino
myFile.close();
```

## Tiefergehende Informationen

Beachten Sie, dass die SD-Bibliothek standardmäßig nur die Dateien im Textmodus öffnet. Dies bedeutet, dass der Zeilenvorschub am Ende jeder Zeile automatisch hinzugefügt wird. Wenn Sie jedoch binäre Daten speichern möchten, müssen Sie die Datei im Binärmodus öffnen.

Ein weiteres wichtiges Detail ist die Größe der SD-Karte. Die Bibliothek unterstützt keine Karten größer als 32GB, und es wird empfohlen, eine schnelle Karte mit hoher Lese- und Schreibgeschwindigkeit zu verwenden, um die Leistung zu optimieren.

## Siehe auch

Hier sind einige nützliche Links für weitere Informationen über das Schreiben von Textdateien auf einem Arduino:

- Arduino SD-Bibliotheksdokumentation: https://www.arduino.cc/en/reference/SD
- Ein ausführliches Tutorial zum Schreiben von Textdateien auf einem Arduino: https://www.instructables.com/id/How-to-write-files-to-a-SD-Card-using-Arduino/
- Weitere Informationen darüber, wie Sie die SD-Karte am besten formatieren und vorbereiten: https://learn.sparkfun.com/tutorials/sd-cards-and-writing-images