---
title:                "Arduino: Entfernen von Zeichen, die einem Muster entsprechen"
simple_title:         "Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum
In der Programmierung gibt es oft Situationen, in denen bestimmte Zeichen in einem Text oder einer Zeichenkette gelöscht werden müssen. Hierfür gibt es verschiedene Methoden, aber in diesem Blogbeitrag möchten wir die "Zeichen löschen"-Funktion in Arduino genauer betrachten.

# Wie das funktioniert
Hier ist ein einfaches Beispiel, um zu verstehen, wie man Zeichen in Arduino löscht. Nehmen wir an, wir haben die Zeichenfolge "Arduino ist cool!" und möchten das Ausrufezeichen am Ende entfernen. Hier ist der Code, den wir verwenden können:

```
Arduino ist cool!

void setup() {
  Serial.begin(9600); //initialisiere die serielle Verbindung
  delay(1000); //warte 1 Sekunde
  Serial.println("Arduino ist cool!"); //gib den ursprünglichen Text aus
}

void loop() {
  char inputString[] = "Arduino ist cool!"; //definiere die Zeichenfolge 
  int length = strlen(inputString); //ermittle die Länge der Zeichenfolge
  inputString[length - 1] = '\0'; //setze das letzte Zeichen auf Null
  Serial.println(inputString); //gib den Text ohne das Ausrufezeichen aus
  delay(1000); //warte 1 Sekunde
}
```

Die Ausgabe, die wir bekommen, lautet: "Arduino ist cool". Das Ausrufezeichen wurde erfolgreich gelöscht.

# Tiefere Einblicke
Die Funktion `strlen()` wird verwendet, um die Länge der Zeichenfolge zu ermitteln. Wir ziehen 1 von der Länge ab, um auf das letzte Zeichen zuzugreifen, da Indizes in Arduino bei 0 beginnen. Das letzte Zeichen wird dann auf Null gesetzt, was in C als Zeichen für das Ende einer Zeichenfolge gilt.

Es ist wichtig zu beachten, dass diese Methode das letzte Zeichen der Zeichenfolge löscht, unabhängig davon, welches Zeichen es ist. Wenn also unsere ursprüngliche Zeichenfolge "Arduino ist cool?" wäre, würde das Fragezeichen gelöscht werden.

# Siehe auch
- [Arduino offizielle Website](https://www.arduino.cc/)
- [Offizielle Arduino Referenz zu Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Einführung in Arduino Programmierung](https://www.makerblog.at/2015/01/eine-einfuhrung-in-die-arduino-programmierung/)