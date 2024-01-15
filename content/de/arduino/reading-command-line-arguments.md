---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Arduino: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Hast du dich jemals gefragt, wie du deinen Arduino dazu bringen kannst, auf Befehle zu reagieren, die du über die Befehlszeile eingibst? Diese Fähigkeit kann dir helfen, deinen Arduino noch vielseitiger und anpassungsfähiger zu machen. In diesem Artikel zeigen wir dir, wie du Befehlszeilenargumente in deinen Arduino-Sketches einlesen kannst.

# Wie geht's

Um Befehlszeilenargumente in deinen Arduino-Sketch einzulesen, musst du zuerst die Bibliothek "CommandLine" herunterladen und installieren. Öffne dazu die Bibliotheksverwaltung in der Arduino-IDE (Strg+Umschalt+I), suche nach "CommandLine" und klicke auf "Installieren".

Als nächstes musst du die Bibliothek in deinem Sketch einbinden. Füge dazu am Anfang deines Codes die folgende Zeile hinzu:

```Arduino
#include <CommandLine.h>
```

Dann erstellst du eine Instanz der CommandLine-Klasse und definierst einen Befehl, den dein Arduino erkennen und darauf reagieren soll:

```Arduino
CommandLine cmd;
cmd.addCommand("blink", blink); // "blink" ist der Befehl, der erkannt werden soll und "blink" ist der Name der Funktion, die ausgeführt werden soll
```

Jetzt musst du nur noch in der Funktion "loop()" die Methode "cmd.read()" aufrufen, damit dein Arduino auf Befehle wartet:

```Arduino
void loop() {
    cmd.read();
}
```

Wenn du nun den String "blink" über die Befehlszeile eingibst und die Eingabetaste drückst, wird die Funktion "blink()" ausgeführt. In diesem Beispiel würde dies bedeuten, dass die eingebaute LED deines Arduinos blinkt.

# Tiefgehende Analyse

Du fragst dich vielleicht, wie die Bibliothek "CommandLine" die Befehlszeilenargumente einliest und erkennt. Sie nutzt die "Serial"-Klasse, um die Eingabe über die serielle Schnittstelle zu empfangen und zu verarbeiten. Dabei wird der empfangene String in einzelne Wörter aufgeteilt und mit den eingestellten Befehlen verglichen. Wenn ein match gefunden wird, wird die entsprechende Funktion ausgeführt.

Probiere ruhig verschiedene Befehle aus und schau dir den Beispiel-Code in der Bibliotheksdokumentation an, um zu sehen, wie du sie in deinem Sketch einsetzen kannst.

# Siehe auch

- [Bibliotheksdokumentation "CommandLine"](https://github.com/nakardo/CommandLine/) (Englisch)
- [Serial-Klasse in der Arduino-Referenz](https://www.arduino.cc/reference/en/language/functions/communication/serial/) (Englisch)
- [Video-Tutorial zur Verwendung von Befehlszeilenargumenten am Beispiel von LEDs](https://www.youtube.com/watch?v=MSbbo2YzYck) (Englisch)