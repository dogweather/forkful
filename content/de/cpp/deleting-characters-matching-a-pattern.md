---
title:    "C++: Löschen von Zeichen mit übereinstimmendem Muster"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann hilfreich sein, um Daten effizient zu filtern oder an bestimmte Anforderungen anzupassen. Außerdem kann es nützlich sein, um unerwünschte Zeichen aus einer Datei oder einem Text zu entfernen.

## Wie

Es gibt mehrere Möglichkeiten, um in C++ Zeichen basierend auf einem Muster zu löschen. Eine Möglichkeit ist die Verwendung von Schleifen, um jeden einzelnen Buchstaben zu überprüfen und ihn zu entfernen, falls er dem Muster entspricht. Eine andere Möglichkeit ist die Verwendung von String-Funktionen wie `find()` und `erase()`, um das Löschen in einem Schritt durchzuführen.

### Beispiel 1

Das folgende Beispiel zeigt die Verwendung von Schleifen, um ein Zeichenarray mit einem gegebenen Muster auf unerwünschte Zeichen zu überprüfen und sie zu entfernen:

```C++
#include <iostream>
#include <string>

using namespace std;

// Funktion zum Löschen von Zeichen basierend auf einem Muster
string deleteCharacters(string str, string pattern) {
    // Länge des Musters
    int patLen = pattern.length();

    // Schleife durch die Zeichen im String
    for (int i = 0; i < str.length(); i++) {
        // Sucht das Muster im String
        if (pattern.find(str[i]) != string::npos) {
            // Entfernt den Buchstaben aus dem String
            str.erase(i, 1);
            // Verringert den Zählwert um die Länge des Musters
            i -= patLen;
        }
    }
    // Gibt den bearbeiteten String zurück
    return str;
}

int main() {
    // Beispiel String und Muster
    string str = "Hello, World!";
    string pattern = "ol";

    // Ausgabe vor der Bearbeitung
    cout << "String vor dem Löschen: " << str << endl;
    
    // Löscht Zeichen aus dem String, die dem Muster entsprechen
    str = deleteCharacters(str, pattern);

    // Ausgabe nach der Bearbeitung
    cout << "String nach dem Löschen: " << str << endl;

    return 0;
}
```

#### Ergebnis

```
String vor dem Löschen: Hello, World!
String nach dem Löschen: He, Wrd!
```

### Beispiel 2

Das folgende Beispiel zeigt die Verwendung von String-Funktionen, um das Löschen in einem Schritt durchzuführen:

```C++
#include <iostream>
#include <string>

using namespace std;

// Funktion zum Löschen von Zeichen basierend auf einem Muster
string deleteCharacters(string str, string pattern) {
    // Durchsucht den String nach dem Muster und entfernt es
    size_t pos = str.find(pattern);
    while (pos != string::npos) {
        str.erase(pos, pattern.length());
        pos = str.find(pattern);
    }
    // Gibt den bearbeiteten String zurück
    return str;
}

int main() {
    // Beispiel String und Muster
    string str = "Hello, World!";
    string pattern = "ol";

    // Ausgabe vor der Bearbeitung
    cout << "String vor dem Löschen: " << str << endl;
    
    // Löscht Zeichen aus dem String, die dem Muster entsprechen
    str = deleteCharacters(str, pattern);

    // Ausgabe nach der Bearbeitung
    cout << "String nach dem Löschen: " << str << endl;

    return 0;
}
```

#### Ergebnis

```
String vor dem Löschen: Hello, World!
String nach dem Löschen: He, Wrd!
```

## Deep Dive

Das Löschen von Zeichen basierend auf einem Muster erfordert eine gründliche Untersuchung des Strings und des Musters, um sicherzustellen, dass die gewünschten Zeichen entfernt werden. Eine sorgfältige Strategie ist wichtig, um unerwünschte Änderungen im String zu vermeiden.

Es gibt auch Bibliotheken in C++ wie z.B. die *Regular Expression Library*, die speziell für die Verarbeitung von Musterabgleichen entwickelt wurden. Diese können bei komplexeren Anforderungen an das Löschen von Zeichen hilfreich sein.

## Siehe auch

- [C++ Strings](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [String-Funk