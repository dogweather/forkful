---
title:    "Arduino: String in Kleinbuchstaben umwandeln"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Möglicherweise möchtest du eine sensorbasierte Anwendung entwickeln oder Daten aus einer externen Quelle verarbeiten. In diesen Fällen kann es erforderlich sein, Texteingaben in Kleinbuchstaben umzuwandeln, um sie besser manipulieren oder vergleichen zu können.

## Wie man eine Zeichenfolge in Kleinbuchstaben umwandelt

Um eine Zeichenfolge in Kleinbuchstaben umzuwandeln, gibt es verschiedene Ansätze. Eine Möglichkeit ist die Verwendung der `toLowerCase()` Funktion. Diese Funktion akzeptiert einen String als Eingabe und gibt denselben String zurück, aber mit allen Buchstaben in Kleinbuchstaben.

```Arduino
// Beispielcode zur Umwandlung in Kleinbuchstaben
String text = "HALLO WELT";
String converted = text.toLowerCase(); // "hallo welt"

// Ausgabe
Serial.println(converted); // "hallo welt"
```

Eine andere Möglichkeit ist die Verwendung der `charAt()` Funktion in Kombination mit der `toLowerCase()` Funktion. Mit `charAt()` kannst du auf einzelne Zeichen in einem String zugreifen und diese dann in Kleinbuchstaben umwandeln.

```Arduino
// Beispielcode zur Umwandlung in Kleinbuchstaben
String text = "HALLO WELT";
String converted = "";

// Schleife durch die einzelnen Zeichen im String
for (int i = 0; i < text.length(); i++) {
  // Zugriff auf aktuelles Zeichen und Umwandlung in Kleinbuchstaben
  converted += tolower(text.charAt(i));
}

// Ausgabe
Serial.println(converted); // "hallo welt"
```

Es gibt auch Bibliotheken wie `String.toLowerCase()` oder `String.toLowerCaseINPlace()`, welche die Umwandlung noch einfacher gestalten.

## Tiefere Einblicke

Beim Umwandeln einer Zeichenfolge in Kleinbuchstaben gibt es einige Dinge, die du beachten solltest. Zum Beispiel können manche Sonderzeichen durch die Umwandlung verändert werden. Auch die Unterstützung von Umlauten und anderen Buchstaben aus verschiedenen Sprachen kann je nach verwendeter Methode variieren.

Es ist immer wichtig, dass du deine Eingaben sorgfältig überprüfst, um unerwartetes Verhalten zu vermeiden. Wenn du beispielsweise nur bestimmte Zeichen oder Zeichenfolgen in Kleinbuchstaben umwandeln möchtest, solltest du dies explizit in deinem Code berücksichtigen.

## Siehe auch

- [String.toLowerCase() Referenz](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/)
- [String.toLowerCaseINPlace() Referenz](https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/tolowercase/)
- [Beispielcode für die Umwandlung von Strings in Kleinbuchstaben](https://forum.arduino.cc/t/solved-how-to-make-string-lowercase/603746)