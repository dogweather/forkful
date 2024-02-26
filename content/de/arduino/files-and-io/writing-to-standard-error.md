---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:21.577065-07:00
description: "Das Schreiben auf den Standardfehler (stderr) in der Arduino-Programmierung\
  \ bedeutet, Fehlermeldungen und Diagnosen in einen separaten Kanal zu leiten, um\u2026"
lastmod: '2024-02-25T18:49:51.211599-07:00'
model: gpt-4-0125-preview
summary: "Das Schreiben auf den Standardfehler (stderr) in der Arduino-Programmierung\
  \ bedeutet, Fehlermeldungen und Diagnosen in einen separaten Kanal zu leiten, um\u2026"
title: Schreiben auf Standardfehler
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben auf den Standardfehler (stderr) in der Arduino-Programmierung bedeutet, Fehlermeldungen und Diagnosen in einen separaten Kanal zu leiten, um zu verhindern, dass sie sich mit der Standardausgabe (stdout) vermischen. Programmierer tun dies, um normale Programmausgaben von Fehlermeldungen zu unterscheiden und das Debugging sowie die Log-Analyse zu erleichtern.

## Wie:

Arduino unterscheidet nicht nativ zwischen Standardausgabe und Standardfehler, wie es bei konventionellen Computersystemen der Fall ist. Sowohl die Methoden `Serial.print()` als auch `Serial.println()` schreiben auf denselben seriellen Ausgang, der typischerweise im Serial Monitor der Arduino IDE angezeigt wird. Wir können jedoch das Schreiben auf stderr emulieren, indem wir Fehlermeldungen speziell formatieren oder sie auf einen alternativen Ausgang leiten, wie eine Datei auf einer SD-Karte oder über eine Netzwerkverbindung.

Um stderr zu emulieren, können Sie Fehlermeldungen mit einem Tag wie "ERROR:" präfixen, um sie im Serial Monitor zu unterscheiden:

```cpp
void setup() {
  Serial.begin(9600); // Initialisiere die serielle Kommunikation mit einer Baudrate von 9600
}

void loop() {
  int result = someFunction();
  if (result == -1) {
    // Emulation von stderr durch Voranstellen der Fehlermeldung
    Serial.println("ERROR: Die Funktion konnte nicht ausgeführt werden.");
  } else {
    Serial.println("Die Funktion wurde erfolgreich ausgeführt.");
  }
  delay(1000); // Warte eine Sekunde, bevor die Schleife neu startet
}

int someFunction() {
  // Eine Dummy-Funktion, die bei einem Fehler -1 zurückgibt
  return -1;
}
```

Eine beispielhafte Ausgabe im Serial Monitor der Arduino IDE könnte so aussehen:

```
ERROR: Die Funktion konnte nicht ausgeführt werden.
```

Für Projekte, die einen ausgefeilteren Ansatz erfordern, einschließlich das Schreiben auf unterschiedliche physische Ausgänge, kann die Verwendung von Drittanbieterbibliotheken oder zusätzlicher Hardware notwendig sein. Zum Beispiel erfordert das Protokollieren von Fehlermeldungen auf einer SD-Karte die `SD`-Bibliothek:

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("ERROR: Initialisierung der SD-Karte fehlgeschlagen!");
    return;
  }
  
  myFile = SD.open("error.log", FILE_WRITE);
  if (myFile) {
    myFile.println("ERROR: Die Funktion konnte nicht ausgeführt werden.");
    myFile.close(); // Stelle sicher, dass die Datei geschlossen wird, um den Inhalt zu speichern
  } else {
    Serial.println("ERROR: Öffnen von error.log fehlgeschlagen!");
  }
}

void loop() {
  // Dein Hauptcode würde hier stehen
}
```

Mit diesem Ansatz trennen Sie physisch die normale Programmausgabe und Fehlermeldungen, indem Sie letztere in eine `error.log`-Datei auf einer SD-Karte leiten, was post-mortem-Analysen ohne Überfüllung des primären Ausgabekanals ermöglicht.
