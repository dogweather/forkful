---
title:                "Arduino: Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum
Wenn Sie mit Arduino programmieren, kann es manchmal nützlich sein, eine eingegebene Zeichenkette in Kleinbuchstaben umzuwandeln. Dies kann hilfreich sein, wenn Sie Benutzereingaben verarbeiten oder mit verschiedenen Sensoren und Aktoren kommunizieren möchten. In diesem Blogbeitrag zeigen wir Ihnen, wie Sie eine Zeichenkette in Kleinbuchstaben umwandeln und warum dies wichtig ist. 

## Wie geht das
Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, können Sie die Arduino-Funktion `toLowerCase()` verwenden. Diese Funktion akzeptiert eine Zeichenkette als Argument und gibt eine neue Zeichenkette zurück, in der alle Buchstaben in Kleinbuchstaben umgewandelt sind. Schauen wir uns ein Beispiel an: 

```Arduino
String text = "HALLO";
text = text.toLowerCase();
Serial.println(text);
```

In diesem Beispiel erstellen wir eine neue Zeichenkette namens `text` und weisen ihr den Wert "HALLO" zu. Dann rufen wir die `toLowerCase()` Funktion auf und weisen das Ergebnis erneut der Variable `text` zu. Schließlich geben wir die neue Zeichenkette über die Serielle Schnittstelle aus. Das Ergebnis wäre "hallo". 

## Tiefergehend
Die `toLowerCase()` Funktion basiert auf der ASCII-Tabelle und wandelt jeden Buchstaben in seiner entsprechenden Kleinbuchstaben-Position um. Dies bedeutet, dass auch Sonderzeichen und Zahlen nicht berücksichtigt werden und unverändert bleiben. Wenn Sie jedoch eine umfassendere Funktion benötigen, können Sie die Bibliothek "StringToLower" verwenden, die speziell für die Arduino-Plattform entwickelt wurde. Diese Bibliothek unterstützt auch Unicode-Zeichen und bietet zusätzliche Optionen für die Umwandlung in Groß- oder Kleinbuchstaben. 

## Siehe auch
- [Arduino Reference:toLowerCase()](https://www.arduino.cc/reference/de/language/functions/communication/tostringtolower/)
- [String toLower() Funktion - Arduino Forum (DE)](https://forum.arduino.cc/index.php?topic=422808.0)
- [StringToLower Bibliothek für Arduino](https://github.com/wontonst/StringToLower)