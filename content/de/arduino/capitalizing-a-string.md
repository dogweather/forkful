---
title:                "Arduino: Einen String großschreiben"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Ändern der Groß- und Kleinschreibung eines Strings kann in der Programmierung nützlich sein, um eine einheitliche Darstellung von Texten zu gewährleisten oder bestimmte Algorithmen anzuwenden. Um dies in Arduino zu erreichen, muss der String in ein Array von Buchstaben aufgeteilt und das entsprechende ASCII-Zeichen geändert werden.

## Wie man es macht

Diese Funktion akzeptiert einen String als Parameter und gibt den gleichen String mit geänderten Groß- und Kleinschreibung zurück.

```Arduino
String capitalizeString(String str) {
  
  // Loop through the characters of the string
  for (int i = 0; i < str.length(); i++) {
    
    char c = str.charAt(i);
    
    // Check if the character is a lowercase letter
    if (c >= 'a' && c <= 'z') {
      
      // Convert it to uppercase by subtracting 32 from its ASCII value
      c = c - 32;
    }
    
    // Replace the character in the string with the new character
    str.setCharAt(i, c);
  }
  
  // Return the capitalized string
  return str;
}

void setup() {
  // Initialize a string
  String myString = "hallo welt";
  
  // Print the original string
  Serial.println(myString);
  
  // Call the capitalizeString function and store the result
  String capitalizedString = capitalizeString(myString);
  
  // Print the capitalized string
  Serial.println(capitalizedString);
}

void loop() {
  // Do nothing
}
```

Die Ausgabe dieses Codes wird sein:

```
hallo welt
HALLO WELT
```

## Tiefergehende Einblicke

Die Änderung der Groß- und Kleinschreibung eines Strings kann auch durch die Verwendung von vordefinierten Funktionen wie `toUpperCase()` und `toLowerCase()` erreicht werden. Diese Funktionen können auf einen einzelnen Buchstaben oder auf den gesamten String angewendet werden. 

In dieser Funktion verwenden wir den ASCII-Code, um die Groß- und Kleinschreibung zu ändern, da dies effizienter ist als die Verwendung von vordefinierten Funktionen. Der ASCII-Code ist eine numerische Darstellung von Buchstaben, Zahlen und Sonderzeichen, die von Computern verwendet wird. Durch Hinzufügen oder Subtrahieren eines bestimmten Werts von diesem Code können wir die Groß- oder Kleinschreibung eines Buchstabens ändern. Zum Beispiel ist der ASCII-Code für den Buchstaben 'a' 97 und für den Buchstaben 'A' 65.

## Siehe auch

- [ASCII-Tabelle](https://www.asciitable.com/)
- [String in ein Array von Buchstaben aufteilen](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/split/)
- [Official Arduino Reference](https://www.arduino.cc/reference/en/)