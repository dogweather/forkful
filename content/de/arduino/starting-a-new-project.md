---
title:    "Arduino: Eine neue Projektarbeit beginnen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Warum

Wenn du dich für Arduino und die Welt des programmierbaren Mikrocontrollers interessierst, dann hast du sicherlich schon von den vielen Möglichkeiten gehört, die dieses Board bietet. Von der Steuerung von Robotern bis hin zur Automatisierung von Haushaltsgeräten - die Anwendungsmöglichkeiten von Arduino sind endlos. Durch das Schreiben eigener Programme kannst du deine eigene elektronische Schaltung bauen und steuern. Wenn du also auf der Suche nach einem neuen spannenden Projekt bist, dann ist Arduino auf jeden Fall einen Versuch wert.

## Wie

Um mit Arduino Programmieren zu beginnen, benötigst du zunächst das Arduino-Board, einen Computer und einen USB-Kabel. Du kannst auch zusätzliche Komponenten wie LEDs, Sensoren oder Motoren verwenden, um dein Projekt noch interessanter zu gestalten.

Um deine Programmierkenntnisse aufzufrischen, schauen wir uns ein einfaches Beispiel an, bei dem wir eine LED zum Blinken bringen. Schritt für Schritt:

1. Beginne zunächst mit dem Anschließen deines Arduino-Boards an deinen Computer über das USB-Kabel.
2. Öffne die Arduino-Entwicklungsumgebung (IDE) auf deinem Computer und erstelle ein neues Sketch (Programm) durch Klicken auf Datei > Neu.
3. Gib deinem Sketch einen Namen und speichere ihn. Es wird automatisch eine Datei mit der Erweiterung `.ino` erstellt.
4. Nun können wir mit dem eigentlichen Programmieren beginnen. Zunächst müssen wir festlegen, welche Pin-Nummer wir für unsere LED verwenden wollen. Dazu deklarieren wir eine Variable `ledPin` im `setup`-Teil unseres Sketches. Zum Beispiel: `int ledPin = 8;`
5. Im nächsten Schritt müssen wir angeben, dass dieser Pin als Ausgang verwendet wird, damit wir die LED steuern können. Dazu fügen wir im `setup`-Teil die folgende Zeile hinzu: `pinMode(ledPin, OUTPUT);`
6. Jetzt können wir im `loop`-Teil unseres Sketches den Code schreiben, um die LED ein- und auszuschalten. Wir verwenden die Funktion `digitalWrite(ledPin, HIGH)` um die LED einzuschalten und `digitalWrite(ledPin, LOW)` um sie auszuschalten.
7. Füge den Code zum Blinken der LED in den `loop`-Teil deines Sketches hinzu. Zum Beispiel: 
```
digitalWrite(ledPin, HIGH);
delay(1000);
digitalWrite(ledPin, LOW);
delay(1000);
```
8. Jetzt kannst du dein Sketch hochladen, indem du auf den Pfeil in der Werkzeugleiste der Arduino IDE klickst. Sobald der Code erfolgreich hochgeladen wurde, sollte deine LED blinken!

## Tiefer Einblick

Das war ein einfaches Beispiel für Arduino-Programmierung, aber es gibt noch viele weitere Möglichkeiten und Funktionen, die du erkunden kannst. Um noch komplexere Projekte durchzuführen, kannst du dich mit der Programmiersprache C++ auseinandersetzen, da Arduino-Code letztendlich in C++ übersetzt wird. Außerdem gibt es eine Vielzahl an Tutorials, Beispielen und einer aktiven Community, die dir bei Fragen oder Problemen zur Seite steht.

Ein wichtiger Aspekt bei der Programmierung ist es, sich auf der offiziellen Arduino-Website über die verschiedenen Boards und Komponenten zu informieren, um sicherzustellen, dass du die richtigen Materialien für dein Projekt auswählst. Außerdem ist es ratsam, ein paar Grundlagen der Elektronik zu verstehen, um die Schaltungen richtig aufzubauen.

## Siehe auch

Hier sind einige nützliche Links, um deine Arduino-Reise fortzusetzen:

- [Offizielle Arduino-Website](https://www.arduino.cc/)
- [Liste der verfügbaren Boards](https://www.arduino.cc/en/Main/Products)
- [Arduino-Tutorial für Anfänger](https://www.arduino.cc/en/Tutorial/HomePage)
- [Arduino-Forum](https://forum.arduino.cc/)

Viel Spaß beim Programmieren mit Arduino!