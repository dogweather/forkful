---
title:                "Fehlersuchausgabe drucken"
html_title:           "Arduino: Fehlersuchausgabe drucken"
simple_title:         "Fehlersuchausgabe drucken"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Beim Programmieren von Arduino-Projekten ist es wichtig, die Funktionen und Abläufe im Blick zu behalten. Um eventuelle Fehler zu beheben oder Probleme zu verstehen, kann es hilfreich sein, Debug-Ausgaben zu nutzen. Diese geben Auskunft darüber, welcher Codeblock ausgeführt wurde und welche Werte bestimmte Variablen haben.

## Wie funktioniert es?

Es gibt verschiedene Möglichkeiten, um Debug-Ausgaben in Arduino zu integrieren. Die einfachste Variante ist die Verwendung der Funktion "Serial.print()". Diese gibt einen Text oder eine Variable auf dem seriellen Monitor aus.

Beispiel:

```arduino
int temperatur = 25;
Serial.print("Die aktuelle Temperatur beträgt: ");
Serial.println(temperatur);
```

Ausgabe:

Die aktuelle Temperatur beträgt: 25

Alternativ kann auch die Funktion "Serial.println()" genutzt werden, um direkt einen Zeilenumbruch anzuhängen.

Neben Zahlen können auch Buchstaben und andere Zeichen ausgegeben werden. Dafür muss die gewünschte Variable in Anführungszeichen gesetzt werden.

Beispiel:

```arduino
char zeichen = 'A';
Serial.print("Das eingegebene Zeichen ist: ");
Serial.println(zeichen);
```

Ausgabe:

Das eingegebene Zeichen ist: A

## Tiefer Einblick

Neben einfachen Debug-Ausgaben können auch komplexere Informationen auf dem seriellen Monitor ausgegeben werden. Dafür gibt es die Funktion "Serial.printf()". Mit dieser können mehrere Variablen in einem String formatiert ausgegeben werden.

Beispiel:

```arduino
int a = 5;
float b = 3.14;
String c = "Hallo Welt";
Serial.printf("Der Wert von a ist %d, b ist %.2f und c ist %s", a, b, c);
```

Ausgabe:

Der Wert von a ist 5, b ist 3.14 und c ist Hallo Welt

Durch die verschiedenen Formatierungszeichen können die Ausgaben übersichtlich gestaltet werden.

## Siehe auch

- [Offizielle Dokumentation zu Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Weiterführende Tipps zum Debuggen von Arduino-Projekten](https://create.arduino.cc/projecthub/Ritchie1337/how-to-debug-poorly-behaving-arduino-projects-129b12)
- [Video-Tutorial zu Debug-Ausgaben in Arduino](https://www.youtube.com/watch?v=nqPslqpjlLI)