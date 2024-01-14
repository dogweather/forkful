---
title:                "C++: Umwandeln eines Strings in Kleinschreibung"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Strings in Kleinbuchstaben kann in verschiedenen Situationen nützlich sein, wie zum Beispiel bei der Eingabe von Benutzernamen oder bei der Vergleich von Strings. Es ermöglicht auch eine konsistente Formatierung von Text.

## Wie man es tut

Das Konvertieren eines Strings in Kleinbuchstaben kann auf verschiedene Arten erreicht werden. Eine Möglichkeit ist die Verwendung von C++ Standardbibliotheksfunktionen, wie z.B. `tolower()`. Hier ist ein Beispielcode, der einen benutzergespeicherten String in Kleinbuchstaben konvertiert:

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
  // Benutzereingabe abfragen
  std::cout << "Geben Sie einen String ein: ";
  std::string input;
  std::getline(std::cin, input);
  
  // Umwandlung in Kleinbuchstaben
  std::transform(input.begin(), input.end(), input.begin(), ::tolower);
  
  // Ausgabe des konvertierten Strings
  std::cout << "Konvertierter String: " << input << std::endl;
  
  return 0;
}
```

### Beispieloutput

```
Geben Sie einen String ein: HALLO WELT
Konvertierter String: hallo welt
```

Es gibt auch andere Möglichkeiten, einen String in Kleinbuchstaben zu konvertieren, wie z.B. die Verwendung von Schleifen und ASCII-Wertmanipulation. Hier ist ein Beispielcode, der dasselbe Ergebnis wie oben erzielt:

```C++
#include <iostream>
#include <string>

int main() {
  // Benutzereingabe abfragen
  std::cout << "Geben Sie einen String ein: ";
  std::string input;
  std::getline(std::cin, input);
  
  // Umwandlung in Kleinbuchstaben
  for (int i = 0; i < input.length(); i++) {
    if (input[i] >= 65 && input[i] <= 90) {
      input[i] = input[i] + 32;
    }
  }
  
  // Ausgabe des konvertierten Strings
  std::cout << "Konvertierter String: " << input << std::endl;
  
  return 0;
}
```

Es ist wichtig zu beachten, dass diese Methode möglicherweise nicht für alle Sonderzeichen und diakritischen Zeichen funktioniert und daher nicht immer zuverlässig ist.

## Tief einsteigen

Die grundlegende Idee hinter der Konvertierung eines Strings in Kleinbuchstaben besteht darin, den ASCII-Wert jedes Zeichens im String zu manipulieren. ASCII (American Standard Code for Information Interchange) ist ein Zeichensatz, der jedem Zeichen eine numerische Darstellung zuweist. Die Kleinbuchstaben im ASCII-Zeichensatz beginnen bei 97 und enden bei 122. Wenn wir also zum ASCII-Wert jedes Großbuchstaben 32 addieren, erhalten wir den entsprechenden Kleinbuchstaben.

Eine Alternative zur ASCII-Wertmanipulation ist die Verwendung von `tolower()` aus der C++ Standardbibliothek. Diese Funktion gibt das entsprechende Kleinbuchstaben-Äquivalent zurück, wenn ein Großbuchstabe übergeben wird.

Es ist auch wichtig zu beachten, dass die Ergebnisse der Konvertierung von Strings in Kleinbuchstaben von der verwendeten Sprache abhängen können. Wenn bestimmte Sonderzeichen oder diakritische Zeichen verwendet werden, ist eine manuelle Manipulation möglicherweise nicht ausreichend und es kann erforderlich sein, spezielle Bibliotheken oder Funktionen zu verwenden.

## Siehe auch

Weitere Informationen und Beispiele zur Konvertierung von Strings in Kleinbuchstaben finden Sie unter:

- https://www.cplusplus.com/reference/cctype/tolower/
- https://www.geeksforgeeks.org/c-program-convert-string-lower-case/
- https://www.techiedelight.com/convert-string-to-uppercase-lowercase-cplusplus/
- https://www.geeksforgeeks.org/c-program-convert-string-lower-case/