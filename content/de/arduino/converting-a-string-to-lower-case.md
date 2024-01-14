---
title:                "Arduino: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung kann es oft nützlich sein, Text in Kleinbuchstaben umzuwandeln. Dies kann zum Beispiel bei der Eingabe von Benutzernamen oder Passwörtern hilfreich sein, um grundsätzlich unterschiedliche Eingaben zu vermeiden. In diesem Blogbeitrag werden wir uns ansehen, wie man mit Arduino eine Zeichenkette in Kleinbuchstaben umwandelt und warum dies nützlich sein kann.

## Wie geht das?

Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, können wir die Funktion `toLowerCase()` verwenden. Diese Funktion ist Teil der Arduino String Library und wandelt jedes Zeichen in der Zeichenkette in seinen entsprechenden Kleinbuchstaben um.

```Arduino
String text = "HALLO WELT!";
text.toLowerCase();
Serial.println(text);
```

Im obigen Beispiel haben wir den String "HALLO WELT!" zunächst als Variable `text` deklariert und dann die `toLowerCase()` Funktion aufgerufen. Wenn wir nun diesen String mit `Serial.println()` ausgeben, wird er als "hallo welt!" erscheinen.

## Weitergehende Informationen

Es ist wichtig zu beachten, dass die `toLowerCase()` Funktion nur den ASCII-Bereich abdeckt, was bedeutet, dass alle Zeichen außerhalb dieses Bereichs nicht in Kleinbuchstaben umgewandelt werden. Zudem ist diese Funktion sprachabhängig, d.h. sie kann nicht mit allen Sprachen und Zeichensätzen verwendet werden.

Wenn du größer Kontrolle über die Umwandlung von Zeichenketten haben möchtest, kannst du auch selbst eine Funktion schreiben, die jeden Buchstaben mit Hilfe der ASCII-Werte umwandelt. Eine solche Funktion könnte etwa so aussehen:

```Arduino
String convertToLowerCase(String str) {
  String result = "";

  for (int i = 0; i < str.length(); i++) {
    int ascii = str[i];
    if (ascii >= 65 && ascii <= 90) {
      ascii += 32;
    }
    result += (char)ascii;
  }

  return result;
}
```

In dieser Funktion wird die übergebene Zeichenkette `str` durchlaufen und jeder Buchstabe in seinen entsprechenden Kleinbuchstaben umgewandelt. Dabei wird geprüft, ob der ASCII-Wert des aktuellen Zeichens zwischen 65 und 90 liegt, was dem Bereich der Großbuchstaben entspricht. Wenn ja, wird der Wert um 32 erhöht, was dem entsprechenden Kleinbuchstaben entspricht. Zum Schluss wird die umgewandelte Zeichenkette `result` zurückgegeben.

## Siehe auch

- [Arduino String Library](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/)
- [ASCII - American Standard Code for Information Interchange](https://www.ascii-code.com/)
- [ASCII - Tabelle mit allen ASCII Zeichen](https://de.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)