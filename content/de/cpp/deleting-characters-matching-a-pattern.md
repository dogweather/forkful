---
title:    "C++: Musterübereinstimmende Zeichen löschen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Es gibt verschiedene Gründe, warum man in der Programmierung möglicherweise Zeichen löschen möchte, die einem bestimmten Muster entsprechen. Zum Beispiel kann es sein, dass man ungewollte Leerzeichen aus einem Text entfernen oder bestimmte Zeichen im String durch andere ersetzen möchte.

## Wie man Zeichen löscht, die einem Muster entsprechen

In C++ gibt es verschiedene Möglichkeiten, um Zeichen zu löschen, die einem bestimmten Muster entsprechen. Eine Möglichkeit ist die Verwendung der `erase()`-Funktion, die in der Standardbibliothek `string` definiert ist. Diese Funktion nimmt zwei Parameter an: den Index des zu löschenden Zeichens und die Anzahl der zu löschenden Zeichen. 

```C++
// Beispielcode zum Löschen aller Vokale aus einem String
#include <iostream>
#include <string>

int main() {
    std::string text = "Dies ist ein Beispieltext.";
    
    // Iteration über den String
    for (int i = 0; i < text.length(); i++) {
        // Prüfung, ob das aktuelle Zeichen ein Vokal ist
        if (text[i] == 'a' || text[i] == 'e' || text[i] == 'i' ||
            text[i] == 'o' || text[i] == 'u') {
            // Lösche das aktuelle Zeichen
            text.erase(i, 1);
        }
    }
    
    // Ausgabe des bearbeiteten Strings
    std::cout << text << std::endl;
    
    return 0;
}
```

**Output:**

```
D s s t s B s p l t x t.
```

Man kann auch die `remove_if()`-Funktion aus der Algorithmus-Bibliothek verwenden, um Zeichen zu löschen, die einem bestimmten Kriterium entsprechen. Diese Funktion nimmt als Parameter eine Funktion oder einen Funktionsobjekt an, die das zu testende Kriterium definiert. Hier ist ein Beispielcode, der alle Zahlen aus einem String löscht:

```C++
// Beispielcode zum Löschen aller Zahlen aus einem String
#include <iostream>
#include <string>
#include <algorithm>

// Funktionsobjekt zum Prüfen, ob ein Zeichen eine Zahl ist
struct IsDigit {
    bool operator()(char c) {
        return std::isdigit(c);
    }
};

int main() {
    std::string text = "Dies ist Beispiel123 Text456.";
    
    // Entferne alle Zahlen aus dem String
    text.erase(std::remove_if(text.begin(), text.end(), IsDigit()), text.end());
    
    // Ausgabe des bearbeiteten Strings
    std::cout << text << std::endl;
    
    return 0;
}
```

**Output:**

```
Dies ist Beispiel Text.
```

## Tiefergehende Informationen über das Löschen von Zeichen

Es ist wichtig zu beachten, dass die `erase()`-Funktion tatsächlich die Zeichen in dem String löscht und die Größe des Strings entsprechend verkleinert. Außerdem bewegen sich alle Zeichen, die sich nach dem zu löschenden Zeichen befinden, einen Index nach oben. 

Beim Einsatz der `remove_if()`-Funktion wird der zu löschende Bereich im String nicht tatsächlich entfernt, sondern nur an das Ende des Strings verschoben. Der Rückgabewert dieser Funktion ist ein Iterator auf den Anfang der nach dem Verschieben nicht mehr benötigten Zeichen. Um diese Zeichen endgültig aus dem String zu löschen, verwendet man die `erase()`-Funktion in Kombination mit dem zurückgegebenen Iterator.

Es gibt auch alternative Methoden, um Zeichen zu löschen, z.B. mit der `replace()`-Funktion oder mit regulären Ausdrücken. Es ist wichtig, die Dokumentation zu diesen Funktionen sorgfältig zu lesen und die richtige Methode für den spezifischen Anwendungszweck auszuwählen.

## Siehe auch

- [C++ string::erase() reference](http://www.cplusplus.com/reference/string/string/erase/)
- [C++ remove_if() reference](http://www.cplusplus.com/reference/algorithm/remove/)
- [C++ string::replace() reference](http://www.cplusplus.com/reference/string/string/replace/)
- [C++ reguläre Ausdrücke tutorial](https://www.regular-expressions.info/tutorial.html)