---
title:                "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben nach **Standardfehler** oder **Standardfehlerausgabe** ist ein wichtiger Teil der Arduino-Programmierung. Es ermöglicht dir, Fehlermeldungen und Diagnoseinformationen auszugeben, während dein Programm läuft. Dies kann dir helfen, Probleme in deinem Code zu identifizieren und zu beheben.

## Wie geht das?

Um zu schreiben, nach **Standardfehler**, musst du zuerst den Serial Monitor öffnen. Dies kannst du tun, indem du auf das Lupensymbol in der oberen rechten Ecke des Arduino-IDE klickst. Dann musst du `Serial.begin(9600);` in deinem `setup()`-Block schreiben, um die serielle Kommunikation zu starten.

Dann kannst du mit dem Befehl `Serial.println()` deine Fehlermeldungen oder Diagnoseinformationen ausgeben. Zum Beispiel:

```
Arduino.setup() {
  Serial.begin(9600);
}

loop() {
  int x = 10;
  if (x > 5) {
    Serial.println("x ist größer als 5");
  }
}
```

Dieser Codeblock gibt die Nachricht "x ist größer als 5" aus, wenn die Bedingung `x > 5` erfüllt ist.

## Tiefere Einblicke

Normalerweise wird nach Standardfehler auf die **Serielle Konsole** geschrieben, aber es ist auch möglich, einen separaten Serial Port zu verwenden, um die Ausgabe zu senden. Dies kann nützlich sein, wenn du mehrere serielle Geräte an deinem Arduino anschließt.

Zusätzlich zu `Serial.println()` gibt es auch `Serial.print()`, welches keine neue Zeile am Ende hinzufügt. Du kannst auch Variablen mit `Serial.print()` ausgeben, indem du den Variablennamen innerhalb der Klammern platzierst, zum Beispiel `Serial.print(x);`.

## Siehe auch

- [Arduino Serial Monitor](https://www.arduino.cc/en/Guide/ArduinoSerialMonitor)
- [Serial-Bibliothek Referenz](https://www.arduino.cc/reference/de/language/functions/communication/serial/)
- [Serial.println() in der Arduino-Dokumentation](https://www.arduino.cc/en/Serial/Println)