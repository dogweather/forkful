---
title:                "Fehlerausgabe drucken"
html_title:           "Arduino: Fehlerausgabe drucken"
simple_title:         "Fehlerausgabe drucken"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# Was & Warum?

Beim Programmieren gibt es oft Fehler, die schwer zu finden sind. Um diese zu beheben, können wir unsere Programme mit sogenannten "Debug Outputs" versehen. Das bedeutet, dass wir unser Programm dazu bringen, während der Ausführung Informationen auszugeben, die uns dabei helfen können, Fehler zu finden und zu beheben.

Warum machen wir das? Nun, wie der Name schon sagt, ist es eine Möglichkeit, Bugs und Fehler in unserem Code zu finden. Durch die Ausgabe von Informationen können wir sehen, wie unser Code ausgeführt wird und wo mögliche Probleme auftreten. Das kann uns eine Menge Zeit und Frustration ersparen!

# Wie geht's?

Es ist ganz einfach, Debug Outputs in unseren Arduino Programmen zu integrieren. Hier ist ein Beispiel für eine einfache Ausgabe von Text auf dem Seriellen Monitor:

```
Arduino.println("Hello World!");
```

Wenn wir diesen Code ausführen, wird "Hello World!" auf dem Seriellen Monitor angezeigt. Wir können jedoch auch Variablen und Werte ausgeben, um genauere Informationen über unseren Code zu erhalten. Hier ist ein Beispiel:

```
int x = 10;
int y = 5;
int z = x + y;
Arduino.print("The value of x is: ");
Arduino.println(x);
Arduino.print("The value of y is: ");
Arduino.println(y);
Arduino.print("The value of z is: ");
Arduino.println(z);
```

In diesem Beispiel werden wir die Werte von x, y und z ausgeben und sehen, wie sie berechnet werden. Wir können auch bedingte Anweisungen verwenden, um nur unter bestimmten Bedingungen Debug-Outputs auszugeben, zum Beispiel:

```
int buttonState = 0;
int buttonPin = 2;
void setup() {
  pinMode(buttonPin, INPUT);
}

void loop() {
  buttonState = digitalRead(buttonPin);
  if (buttonState == HIGH) {
    Arduino.println("Button pressed!");
  }
}
```

Diese Ausgabe wird nur auf dem Seriellen Monitor angezeigt, wenn der Schalter auf Pin 2 gedrückt wird.

# Tiefer einsteigen

Debug Outputs gibt es schon seit den Anfängen des Programmierens. Früher wurden spezielle Geräte wie Drucker und Konsolenterminals verwendet, um die Ausgabe anzuzeigen. Heutzutage verwenden wir häufig den Seriellen Monitor, der in der Arduino-IDE integriert ist, um unsere Ausgabe zu betrachten.

Es gibt auch alternative Methoden, um Debug-Outputs zu integrieren, wie z.B. das Schreiben in eine Datei oder das Debuggen mit externen Tools. Diese können jedoch komplexer sein und erfordern oft spezielle Kenntnisse.

Die Implementierung von Debug Outputs kann auch Auswirkungen auf die Performance unseres Codes haben. Deshalb sollten wir sie in unseren finalen Programmen deaktivieren oder entfernen, um die Ausführungsgeschwindigkeit nicht zu beeinflussen.

# Weitere Informationen

Für weitere Informationen und Beispiele zu Debug Outputs empfehle ich die offizielle Arduino-Dokumentation auf ihrer Website https://www.arduino.cc/reference/en/language/functions/communication/serial/.

Zusätzlich gibt es auch viele hilfreiche Tutorials und Diskussionen in der Arduino-Community, die sich mit diesem Thema beschäftigen. Eine kurze Google-Suche kann viele nützliche Ergebnisse liefern.

Viel Spaß beim Debuggen!