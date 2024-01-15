---
title:                "Ermittlung der Länge eines Strings"
html_title:           "Arduino: Ermittlung der Länge eines Strings"
simple_title:         "Ermittlung der Länge eines Strings"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Warum:

Die Länge einer Zeichenkette (String) ist eine wichtige Information, die wir in vielen Anwendungen benötigen. Zum Beispiel könnte man damit die Anzahl der eingegebenen Zeichen in einem Textfeld überwachen oder die Länge eines Passwortes überprüfen. In diesem Artikel werden wir uns damit beschäftigen, wie man mithilfe von Arduino die Länge einer Zeichenkette bestimmen kann.

Wie geht's:

Um die Länge einer Zeichenkette in Arduino zu finden, gibt es verschiedene Ansätze. Wir werden uns hier auf den einfachsten Weg konzentrieren, indem wir die Funktion "length()" verwenden. Diese Funktion zählt die Anzahl der Zeichen in einer Zeichenkette und gibt das Ergebnis zurück.

```Arduino
String meineZeichenkette = "Hallo Welt!";
int laenge = meineZeichenkette.length(); // laenge = 11
Serial.println(laenge); // gibt 11 aus
```

In diesem Beispiel haben wir zuerst eine Zeichenkette mit dem Text "Hallo Welt!" erstellt und dann die Funktion "length()" auf diese Zeichenkette angewendet. Das Ergebnis wird in der Variablen "laenge" gespeichert und anschließend über die serielle Schnittstelle ausgegeben.

Deep Dive:

Die Funktion "length()" ist sehr einfach zu nutzen und gibt uns schnell die gewünschte Information. Aber was passiert eigentlich im Hintergrund? Wie funktioniert die Funktion?

Die Funktion "length()" ist Teil des String Objekts in der Arduino Programmiersprache. Dieses Objekt beinhaltet alle Funktionen, die auf Zeichenketten angewendet werden können. Die Implementierung dieser Funktion ist eine einfache Schleife, die jedes Zeichen in der Zeichenkette durchläuft und zählt. Am Ende wird die Anzahl der durchlaufenden Schleifendurchgänge zurückgegeben, was der Länge der Zeichenkette entspricht.

Also keine Sorge, wenn du die genaue Funktionsweise der Funktion "length()" nicht verstehst. Wichtig ist, dass du weißt, wie du sie verwenden kannst, um die Länge einer Zeichenkette in Arduino zu bestimmen.

Siehe auch:

- [Arduino Reference - String Length](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)
- [Arduino String Object Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)