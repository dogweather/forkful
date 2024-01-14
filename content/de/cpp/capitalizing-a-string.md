---
title:    "C++: Ein String großschreiben"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Zeichenfolgen ist eine häufige Aufgabe in der Programmierung. Es ist wichtig, um beispielsweise Eingaben vom Benutzer oder von Datenbanken zu formatieren oder um Strings für bestimmte Ausgaben zu konvertieren.

## Wie man es macht

Das Kapitalisieren einer Zeichenfolge in C++ ist einfach. Hier ist ein Beispiel:

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  // Eine Zeichenfolge vom Benutzer eingeben
  string eingabe;
  cout << "Bitte geben Sie eine Zeichenfolge ein: ";
  cin >> eingabe;

  // Die Zeichenfolge in Großbuchstaben umwandeln
  for (int i = 0; i < eingabe.length(); i++) {
    eingabe[i] = toupper(eingabe[i]);
  }

  // Ausgabe der kapitalisierten Zeichenfolge
  cout << "Die kapitalisierte Zeichenfolge ist: " << eingabe << endl;
  return 0;
}
```

#### Output:
```
Bitte geben Sie eine Zeichenfolge ein: Hallo Welt
Die kapitalisierte Zeichenfolge ist: HALLO WELT
```

## Tief einsteigen

Es gibt verschiedene Möglichkeiten, eine Zeichenfolge in C++ zu kapitalisieren. Das obige Beispiel ist nur eine Möglichkeit. Andere Optionen sind die Verwendung von String-Funktionen wie `transform()` oder das Erstellen einer eigenen Funktion.

Die `transform()` Funktion kann auch dazu verwendet werden, eine Zeichenfolge in Kleinbuchstaben oder die erste Buchstabe eines Wortes in Großbuchstaben zu konvertieren. Hier ist ein Beispiel, das die ersten Buchstaben eines Wortes in Großbuchstaben umwandelt:

```C++
#include <iostream>
#include <string>
#include <algorithm>
using namespace std;

// Funktion, die die ersten Buchstaben in Großbuchstaben umwandelt
string erste_buchstaben(string s) {
  s[0] = toupper(s[0]);
  for (int i = 1; i < s.length(); i++) {
    if (s[i-1] == ' ') {
      s[i] = toupper(s[i]);
    }
  }
  return s;
}

int main() {
  // Eine Zeichenfolge vom Benutzer eingeben
  string eingabe;
  cout << "Bitte geben Sie eine Zeichenfolge ein: ";
  getline(cin, eingabe);

  // Ausgabe der Zeichenfolge mit umgewandelten ersten Buchstaben
  cout << "Zeichenfolge mit umgewandelten ersten Buchstaben: " << erste_buchstaben(eingabe) << endl;
  return 0;
}
```

#### Output:
```
Bitte geben Sie eine Zeichenfolge ein: dies ist ein beispiel
Zeichenfolge mit umgewandelten ersten Buchstaben: Dies Ist Ein Beispiel
```

## Siehe auch

- [String-Funktionen in C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [C++: Strings in Großbuchstaben umwandeln](https://stackoverflow.com/questions/7352046/c-strings-to-uppercase)
- [C++: Erste Buchstaben eines Worts in Großbuchstaben umwandeln](https://stackoverflow.com/questions/35758809/c-convert-first-letter-of-every-word-in-a-string-to-uppercase)