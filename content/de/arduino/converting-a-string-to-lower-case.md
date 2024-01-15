---
title:                "Umwandlung eines Strings in Kleinbuchstaben"
html_title:           "Arduino: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum
Die Verwendung von Kleinbuchstaben in Strings kann die Lesbarkeit erhöhen und die Programmierung von Vergleichen erleichtern. Dieser Artikel zeigt, wie man in Arduino eine Zeichenkette in Kleinbuchstaben umwandeln kann.

# Wie geht's
```Arduino
String text = "HALLO WELT";
text.toLowerCase();
Serial.println(text);

// Output: hallo welt
```

Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, muss die Funktion `toLowerCase()` auf die Zeichenkette angewendet werden. Diese Funktion konvertiert jeden Buchstaben in der Zeichenkette in einen Kleinbuchstaben. Das Ergebnis wird dann ausgegeben, wie im obigen Beispiel gezeigt.

Eine andere Möglichkeit ist die Verwendung einer Schleife, um jeden Buchstaben in der Zeichenkette zu durchlaufen und ihn in einen Kleinbuchstaben zu konvertieren.

```Arduino
String text = "HALLO WELT";
String newText = "";

for (int i = 0; i < text.length(); i++) {
  char character = toLowerCase(text.charAt(i));
  newText = newText + character;
}

Serial.println(newText);

// Output: hallo welt
```

In diesem Beispiel wird jeder Buchstabe der Variablen `text` durchlaufen und mit der Funktion `toLowerCase()` in einen Kleinbuchstaben umgewandelt. Der neue Kleinbuchstaben wird dann der Variablen `newText` hinzugefügt. Am Ende wird die neue Zeichenkette ausgegeben.

# Deep Dive
Die Funktion `toLowerCase()` ist in der Arduino String-Klasse definiert und kann auf jede Zeichenkette angewendet werden, die mit `String` deklariert wurde. Diese Funktion wurde jedoch erst mit der Version 1.5.8 der Arduino-IDE eingeführt. Wenn Sie eine ältere Version verwenden, sollten Sie die alternative Methode mit der Schleife verwenden.

Es ist auch wichtig zu beachten, dass die Funktion `toLowerCase()` nur reguläre Buchstaben in Kleinbuchstaben umwandelt. Andere Zeichen, wie Zahlen oder Sonderzeichen, werden nicht verändert.

# Siehe auch
- [Arduino String-Klasse Referenz](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Kleinbuchstaben in Großbuchstaben umwandeln](https://www.arduino.cc/reference/en/language/functions/string/tolowercase/)