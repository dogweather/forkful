---
title:                "Die Länge eines Strings finden"
html_title:           "C++: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Warum

Es ist wichtig, die Länge einer Zeichenkette in C++ zu finden, da dies eine grundlegende Funktion ist, um mit Texten zu arbeiten. Durch das Erlernen dieser Fähigkeit können Sie effektivere und effizientere Programme schreiben.

# Wie man die Länge einer Zeichenkette in C++ findet

Eine Zeichenkette in C++ ist eine Sequenz von Zeichen, die entweder von einem Einfachzeichen oder einer Reihe von Zeichenkonstanten umgeben ist. Um die Länge einer Zeichenkette zu finden, müssen wir die Anzahl der Zeichen zählen, die in der Zeichenkette enthalten sind.

Hier ist ein Beispielcode, um die Länge einer Zeichenkette zu finden:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string zeichenkette = "Hallo Welt!";
  int laenge = zeichenkette.length();

  cout << "Die Länge der Zeichenkette ist: " << laenge << endl;
  return 0;
}
```

Das obige Beispiel zeigt, wie Sie die `length()` Funktion der `string` Klasse verwenden können, um die Länge der Zeichenkette zu finden. Diese Funktion gibt die Anzahl der Zeichen in der Zeichenkette zurück.

Die Ausgabe des oben genannten Codes wird sein:

```
Die Länge der Zeichenkette ist: 11
```

## Tiefergehende Informationen

Bei der Verwendung der `length()` Funktion ist es wichtig zu wissen, dass die Indizierung von Zeichenketten in C++ bei 0 beginnt. Das bedeutet, dass das erste Zeichen in der Zeichenkette an Position 0 steht und nicht an Position 1.

Um auf ein bestimmtes Zeichen in der Zeichenkette zuzugreifen, können Sie die `at()` Funktion verwenden, die ein Zeichen basierend auf einer angegebenen Position zurückgibt. Wenn Sie jedoch versuchen, auf ein Zeichen an einer ungültigen Position zuzugreifen, wird ein Fehler ausgelöst.

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string zeichenkette = "Hallo Welt!";
  
  // Gib das 3. Zeichen in der Zeichenkette aus
  cout << "Das 3. Zeichen ist: " << zeichenkette.at(2) << endl;
  
  // Versuche, auf ein ungültiges Zeichen zuzugreifen
  cout << "Das 20. Zeichen ist: " << zeichenkette.at(19) << endl;
  
  return 0;
}
```

Die Ausgabe des obigen Codes wird wie folgt sein:

```
Das 3. Zeichen ist: l
terminate called after throwing an instance of 'std::out_of_range'
  what():  basic_string::at: __n (which is 19) >= this->size() (which is 11)
```

In diesem Beispiel sehen Sie, wie das Programm wegen des ungültigen Zeichenzugriffs abgebrochen wurde.

# Siehe Auch

- [string::length() Referenz](https://www.cplusplus.com/reference/string/string/length/) 
- [string::at() Referenz](https://www.cplusplus.com/reference/string/string/at/)