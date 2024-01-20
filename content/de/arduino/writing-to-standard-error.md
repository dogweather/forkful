---
title:                "Schreiben auf Standardfehler"
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Schreiben auf Standard Error (stderr) bedeutet, Fehlermeldungen und Diagnoseinformationen getrennt von der normalen Ausgabe (stdout) zu behandeln. Programmierer tun dies, um Fehler leicht zu identifizieren und mit Log-Dateien oder Konsolenausgaben effizienter zu arbeiten.

## So geht's:
Arduino bietet keine native Unterscheidung zwischen stdout und stderr. Wir verwenden `Serial.print` für die normale Ausgabe und definieren eine eigene Funktion für Fehlermeldungen.

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Normal output
  Serial.println("Alles in Ordnung");

  // Error output
  if (analogRead(A0) > 400) {
    errorPrint("Spannung zu hoch!");
  }
}

void errorPrint(String errMsg) {
  Serial.print("ERROR: ");
  Serial.println(errMsg);
}
```

Erwartete Ausgabe:
```
Alles in Ordnung
ERROR: Spannung zu hoch!
```

## Hintergrundwissen:
Standard Error ist eine Standard-Datenstrom, der ursprünglich in UNIX-Systemen eingeführt wurde, um Programmfehler von normalen Ausgaben zu trennen. Arduino selbst unterscheidet nicht zwischen stdout und stderr, aber wir können mit `Serial` beides simulieren. Alternativen wie `stderr`-Streams sind typischer auf Betriebssystem-Ebene oder in komplexeren Programmiersprachen wie C oder Python zu finden, nicht in der Arduino-Welt.

## Siehe auch:
- [Serial - Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Understanding Standard Streams - GNU](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [Error Handling in Arduino - Arduino Project Hub](https://create.arduino.cc/projecthub)