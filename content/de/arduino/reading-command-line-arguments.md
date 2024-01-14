---
title:    "Arduino: Lesen von Befehlszeilenargumenten"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich mit dem Lesen von Befehlszeilenargumenten beschäftigen? Nun, dieses Wissen ist sehr hilfreich, wenn man komplexe Programme schreibt, die auf externe Eingaben reagieren müssen. Es ermöglicht dem Programmierer, die Flexibilität und Interaktion mit dem Benutzer zu verbessern.

## How To
Das Lesen von Befehlszeilenargumenten in Arduino ist relativ einfach. Zuerst muss man ein Objekt der Klasse Serial beginnen, um die serielle Kommunikation zwischen dem Arduino und einem Computer oder einem anderen Gerät zu ermöglichen. Dann können die Befehlszeilenargumente mithilfe der Funktion `Serial.readStringUntil()` gelesen werden, die eine Zeichenkette bis zu einem angegebenen Trennzeichen zurückgibt.

Hier ist ein Beispielcode, der demonstriert, wie man Befehlszeilenargumente mit dem Arduino liest:

```
Arduino initialisieren .....
Serial.begin(9600);     // Starten der seriellen Kommunikation
String input = Serial.readStringUntil('\n');     // Lesen der Argumente bis zur Zeilenumbruch
Serial.print("Die Eingabe war: ");
Serial.println(input);    // Ausgabe der Eingabe über die serielle Schnittstelle
```

Wenn Sie nun "Hello World" über die serielle Schnittstelle senden, wird die Ausgabe "Die Eingabe war: Hello World" erscheinen.

## Deep Dive
Um tiefer in die Materie einzusteigen, ist es wichtig zu wissen, dass Befehlszeilenargumente meistens als Zeichenketten (Strings) gelesen werden müssen und dann entsprechend in andere Datentypen umgewandelt werden müssen, je nachdem, wie sie im Programm verwendet werden sollen.

Es ist auch hilfreich, sicherzustellen, dass die eingegebenen Argumente gültig sind, um unerwartete Fehler zu vermeiden. Dies kann durch die Verwendung von Bedingungen und Vergleichen erreicht werden.

Ein weiterer wichtiger Aspekt ist das Verständnis der Position der Befehlszeilenargumente im Programmablauf. Der Code muss so gestaltet sein, dass die Argumente im richtigen Moment gelesen und verarbeitet werden, um die gewünschten Ergebnisse zu erzielen.

## Siehe auch
1. [Arduino Serial Communication](https://www.arduino.cc/en/Tutorial/SerialCommunication)
2. [String Manipulation in Arduino](https://www.arduino.cc/en/Tutorial/String) 
3. [Understanding Command Line Arguments](https://www.gnu.org/software/libc/manual/html_node/Command_002dLine-Arguments.html)
4. [C++ String Library](https://www.cplusplus.com/reference/string/string/)