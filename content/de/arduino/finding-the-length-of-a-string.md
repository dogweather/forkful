---
title:                "Arduino: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Warum

Ja, warum sollte man sich überhaupt damit beschäftigen, die Länge eines Strings zu finden? Ganz einfach: Strings sind in der Programmierung eine wichtige Datenstruktur, die häufig verwendet wird. Das Finden der Länge eines Strings kann also dabei helfen, bestimmte Probleme zu lösen und den Code effizienter zu gestalten.

##Wie geht das?
Um die Länge eines Strings in Arduino zu finden, gibt es verschiedene Möglichkeiten. Eine einfache Methode wäre, die Funktion `strlen()` zu verwenden, die in der `string.h` Bibliothek enthalten ist. Diese Funktion erwartet den zu prüfenden String als Parameter und gibt als Ergebnis die Anzahl der Zeichen im String zurück.

```Arduino
#include <string.h>

void setup() {
  Serial.begin(9600); // Initialisiere die serielle Schnittstelle
  
  char meinString[] = "Hallo Welt"; // Definiere den String
  
  int laenge = strlen(meinString); // Rufe die Funktion auf
  
  Serial.println("Die Länge des Strings ist: ");
  Serial.println(laenge); // Gib das Ergebnis aus
}

void loop() {
  // Code der loop-Funktion
}
```

Der obige Code würde als Output "Die Länge des Strings ist: 10" ausgeben, da unser String 10 Zeichen lang ist (einschließlich eines Leerzeichens).

Es ist auch möglich, die Länge eines Strings mithilfe einer Schleife zu ermitteln. In diesem Beispiel gehen wir davon aus, dass der String mit einem Nullzeichen (oder auch null-terminierendes Zeichen) am Ende endet, das als Markierung dient. Wir zählen also so lange die Zeichen, bis wir auf das Nullzeichen stoßen.

```Arduino
void setup() {
  Serial.begin(9600); // Initialisiere die serielle Schnittstelle
  
  char meinString[] = "Hallo Welt"; // Definiere den String
  
  int laenge = 0; // Initialisiere die Länge
  
  // Schleife zählt die Zeichen des Strings
  while (meinString[laenge] != '\0') {
    laenge++;
  }

  Serial.println("Die Länge des Strings ist: ");
  Serial.println(laenge); // Gib das Ergebnis aus
}

void loop() {
  // Code der loop-Funktion
}
```

Das Ergebnis wäre hier ebenfalls "Die Länge des Strings ist: 10". Beachte aber, dass die Funktion `strlen()` bereits deutlich effizienter ist und daher empfohlen wird.

##Der tiefe Einblick
Wenn du dich näher mit dem Thema beschäftigen möchtest, gibt es einige Dinge zu beachten. Zum einen gibt es bei der Verwendung von `strlen()` keine Möglichkeit, zu überprüfen, ob der zu prüfende String tatsächlich einen Nullterminator besitzt. Dies kann zu Fehlern führen, wenn der String nicht korrekt erstellt wurde.

Außerdem gibt es unterschiedliche Codierungen (wie z.B. ASCII oder UTF-8), die sich auf die Anzahl der Zeichen und damit auch auf die Länge eines Strings auswirken können. Es ist daher wichtig, die Codierung zu berücksichtigen, wenn es um die Länge von Strings geht.

##Siehe auch
- [Tutorial: Der Umgang mit Strings in Arduino](https://www.arduino.cc/en/Tutorial/TextString)
- [String Reference von Arduino](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- [ASCII-Tabelle](https://www.ascii-code.com/)