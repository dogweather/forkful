---
title:                "Arduino: Schreiben in die Standardfehlerausgabe"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf den Standardfehler ist ein wichtiger Aspekt der Arduino-Programmierung. Es ermöglicht uns, Fehler und Warnungen in unserem Code zu erkennen und zu diagnostizieren.

## Wie man schreibt auf den Standardfehler
Um auf den Standardfehler zu schreiben, verwenden wir die `Serial` Bibliothek und die Funktion `Serial.println()`. Hier ist eine einfache Beispielcode, der "Hello World" auf den Standardfehler ausgibt:

```Arduino
void setup() {
  // initialisiere die serielle Kommunikation
  Serial.begin(9600);
}

void loop() {
  // schreibe auf den Standardfehler
  Serial.println("Hello World");
  delay(1000);
}
```

Die Ausgabe wird im seriellen Monitor angezeigt und sieht wie folgt aus:

```
Hello World
Hello World
Hello World
...
```

## Tiefer blicken
Das Schreiben auf den Standardfehler ist besonders nützlich, wenn wir unser Programm debuggen möchten. Wir können Variablenwerte und andere Informationen ausgeben, um zu verstehen, wo unser Code möglicherweise fehlerhaft ist.

```Arduino
int num = 5;

void setup() {
  // initialisiere die serielle Kommunikation
  Serial.begin(9600);
}

void loop() {
  // schreibe den Wert der Variable auf den Standardfehler
  Serial.println("Der Wert von num ist: " + String(num));
  delay(1000);
}
```

Die Ausgabe im seriellen Monitor lautet nun:

```
Der Wert von num ist: 5
Der Wert von num ist: 5
Der Wert von num ist: 5
...
```

Wir können auch Warnungen oder Fehlermeldungen ausgeben, um auf potenzielle Probleme in unserem Code aufmerksam zu machen:

```Arduino
int num = 0;

void setup() {
  // initialisiere die serielle Kommunikation
  Serial.begin(9600);
}

void loop() {
  // erhöhe den Wert von num
  num++;
  
  // überprüfe, ob es größer als 5 ist
  if (num > 5) {
    // gib eine Warnung auf den Standardfehler aus
    Serial.println("Achtung, num ist größer als 5!");
  }
  
  delay(1000);
}
```

Die Ausgabe im seriellen Monitor sieht nun folgendermaßen aus:

```
Achtung, num ist größer als 5!
Achtung, num ist größer als 5!
Achtung, num ist größer als 5!
...
```

## Siehe auch
- [Arduino Serial Library Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Debugging in Arduino](https://www.arduino.cc/en/Guide/AdvancedDebugging)
- [Einführung in das Schreiben von Fehlern in C++](https://www.learncpp.com/cpp-tutorial/5-8-writing-files/)