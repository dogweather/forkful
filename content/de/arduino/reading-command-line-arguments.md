---
title:                "Das Lesen von Kommandozeilenargumenten."
html_title:           "Arduino: Das Lesen von Kommandozeilenargumenten."
simple_title:         "Das Lesen von Kommandozeilenargumenten."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Was & Warum?

Lesen von Befehlszeilenargumenten ist die Möglichkeit, Eingaben vom Benutzer zu erhalten, während das Programm ausgeführt wird. Programmierer nutzen dies, um ihre Programme interaktiver und flexibler zu gestalten.

Wie geht's:

Um Befehlszeilenargumente in Arduino zu lesen, verwenden Sie die Funktion ```Arduino.read()```, gefolgt von der gewünschten Datentypspezifikation und dem Variablennamen. Zum Beispiel: ```Arduino.read(int input)``` liest eine Ganzzahl (integer) und speichert sie in der Variablen "input". 

Der Codeblock unten zeigt, wie man beim Drücken eines Knopfes auf dem verwendeten Arduino Board eine Eingabe lesen und diese dann ausgeben kann.

```
int buttonPin = 2;    // Definiere den Knopf-Pin
int buttonState = 0;  // Diese Variable wird später benutzt, um den aktuellen Knopf-Status zu speichern

void setup() {
  pinMode(buttonPin, INPUT);  // Definiere den Pin als Eingang
  Serial.begin(9600);  // Starte die serielle Kommunikation
}

void loop() {
  buttonState = digitalRead(buttonPin);  // Lese den aktuellen Knopf-Status
  if (buttonState == HIGH) { // Wenn der Knopf gedrückt wird, führe das folgende aus
    Serial.print("Button wurde gedrückt"); // Gib eine Nachricht auf der seriellen Schnittstelle aus
    // Hier können weitere Aktionen ausgeführt werden, abhängig von der Eingabe des Benutzers
  }
  delay(10); // Warte 10 Millisekunden, um Flackern zu vermeiden
}
```

Deeper Dive:

Die Möglichkeit, Befehlszeilenargumente zu lesen, ist ein wichtiger Teil der Programmierung, der aus der Notwendigkeit entstanden ist, Aktionen durch Benutzereingaben zu steuern. Früher war dies oft nur mit komplizierteren Methoden möglich, wie zum Beispiel über Schalter oder Tastatureingaben. Heutzutage ist das Lesen von Befehlszeilenargumenten eine gängige Methode, um Benutzereingaben zu verarbeiten.

Eine Alternative zur Eingabe von Befehlszeilenargumenten kann das Senden von Daten über eine serielle Schnittstelle von einem anderen Gerät sein. Dies kann jedoch zusätzliche Hardware und Komplexität erfordern.

Die Implementierung des Lesens von Befehlszeilenargumenten kann je nach Programmiersprache und Gerät variieren. In Arduino müssen bestimmte Funktionen verwendet werden, um auf die serielle Schnittstelle und die Eingabeparameter zuzugreifen.

Siehe auch:

- https://www.arduino.cc/reference/de/language/functions/communication/serial/read/
- https://www.arduino.cc/en/Tutorial/Button