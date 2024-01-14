---
title:                "Arduino: Die Länge von einem String finden"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Sich mit der Länge einer Zeichenkette zu beschäftigen, mag auf den ersten Blick vielleicht nicht besonders aufregend klingen. Aber wenn du ein Arduino-Enthusiast bist, wirst du wissen, dass es viele Situationen gibt, in denen du die Länge einer Zeichenkette kennen musst. Zum Beispiel, wenn du eine Benutzer*innen-Eingabe verarbeitest oder Daten über eine serielle Schnittstelle empfängst. Das Wissen über die Länge einer Zeichenkette ist also eine wichtige Fähigkeit, die dir helfen kann, deine Arduino-Programme effizienter zu gestalten.

## Wie gehts

Um die Länge einer Zeichenkette in deinem Arduino-Programm zu finden, gibt es ein paar Schritte, die du befolgen musst. Zunächst einmal musst du die Bibliothek `String` in deinem Code importieren. Diese Bibliothek bietet Funktionen, mit denen du Zeichenketten manipulieren und Eigenschaften wie die Länge abrufen kannst. Dann erstellst du eine Variable vom Datentyp `String` und weist ihr die gewünschte Zeichenkette zu. Um die Länge der Zeichenkette abzurufen, verwendest du die Funktion `length()`, die die Anzahl der Zeichen in der Zeichenkette zurückgibt. Im Folgenden findest du ein Beispielcode, der die Länge einer Zeichenkette berechnet:

```Arduino
#include <String.h>

void setup() {
  // Initialisiere die serielle Kommunikation
  Serial.begin(9600);

  // Erstelle eine Variable "name" vom Typ "String"
  String name = "Arduino";

  // Rufe die Funktion "length()" auf und speichere das Ergebnis in einer Variablen
  int length = name.length();

  // Gib die Länge der Zeichenkette über die serielle Schnittstelle aus
  Serial.println(length);
}

void loop() {
  // Hier kannst du weitere Befehle für die Schleife schreiben
}
```

Das obige Beispiel wird die Länge der Zeichenkette "Arduino" (6) über die serielle Schnittstelle ausgeben. Natürlich kannst du die Funktion `length()` in deinem Code anpassen und auf deine individuellen Bedürfnisse anpassen.

## Tiefer in die Materie eintauchen

Nun, da du weißt, wie du die Länge einer Zeichenkette in deinem Arduino-Programm finden kannst, gibt es noch ein paar Dinge, die du beachten solltest. Zum Beispiel ist es wichtig zu wissen, dass die Funktion `length()` nur die Anzahl der Zeichen in der Zeichenkette zurückgibt und keine Leerzeichen oder Sonderzeichen berücksichtigt. Außerdem solltest du darauf achten, dass deine Zeichenkette nicht zu lang ist, da dies zu Problemen bei der Speicherung und Manipulation führen kann.

Ein nützlicher Tipp ist es auch, dir das sogenannte "Null-Byte" anzuschauen. Dieses unsichtbare Zeichen wird am Ende einer Zeichenkette hinzugefügt und zeigt dem Arduino an, wo die Zeichenkette endet. Wenn du also die Länge einer Zeichenkette manuell zählst, solltest du dieses Null-Byte ausschließen.

Insgesamt ist das Finden der Länge einer Zeichenkette eine wichtige Fähigkeit für jeden Arduino-Programmierer, um effiziente und zuverlässige Programme zu schreiben.

## Siehe auch

- [Offizielle Arduino-Dokumentation zu String-Funktionen](https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/)
- [Tutorial: Strings in Arduino-Programmen](https://maker.pro/arduino/tutorial/how-to-work-with-strings-in-arduino)
- [Erklärvideo: Die Länge einer Zeichenkette finden in Arduino](https://www.youtube.com/watch?v=QsouC7hbQmA)