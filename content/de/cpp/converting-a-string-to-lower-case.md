---
title:                "C++: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Umwandeln von Strings in Kleinbuchstaben ist eine häufige Aufgabe beim Programmieren. Es kann nützlich sein, wenn man zum Beispiel Benutzereingaben einheitlich verarbeiten oder Texte vergleichen möchte. In diesem Blog-Beitrag zeigen wir, wie man Strings in C++ in Kleinbuchstaben umwandeln kann.

## So geht's

Um einen String in Kleinbuchstaben zu konvertieren, gibt es mehrere verschiedene Ansätze. Im Folgenden geben wir euch zwei Beispiele mit verschiedenen Methoden.

### Beispiel 1: Verwendung der transform() -Funktion

```C++
// C++ Code für String-Konvertierung in Kleinbuchstaben
#include <iostream>
#include <algorithm> // transform() Funktion

using namespace std;

int main() {
    string str = "Hallo Welt";
    
    // transform() Funktion um str in Kleinbuchstaben zu konvertieren
    transform(str.begin(), str.end(), str.begin(), ::tolower);

    // Ausgabe des Ergebnisses
    cout << str << endl; // gibt "hallo welt" aus

    return 0;
}
```
In diesem Beispiel verwenden wir die `transform()` Funktion aus der Algorithmus-Bibliothek, um den String in Kleinbuchstaben zu konvertieren. Wichtig ist hierbei, dass wir als letzten Parameter den `::tolower`-Operator angeben, der die entsprechenden Zeichen in Kleinbuchstaben umwandelt.

### Beispiel 2: Manuelle Umwandlung mit Schleife

```C++
// C++ Code für manuelle String-Konvertierung in Kleinbuchstaben
#include <iostream>
#include <cctype> // tolower() Funktion

using namespace std;

int main() {
    string str = "Hallo Welt";

    // Schleife um jeden Buchstaben in Kleinbuchstaben umzuwandeln
    for(int i = 0; i < str.length(); i++) {
        str[i] = tolower(str[i]);
    }

    // Ausgabe des Ergebnisses
    cout << str << endl; // gibt "hallo welt" aus

    return 0;
}
```

In diesem Beispiel nutzen wir die `tolower()` Funktion aus der C-Bibliothek, um jeden Buchstaben des Strings manuell in Kleinbuchstaben zu konvertieren.

## Tiefere Einblicke

Es gibt noch weitere Möglichkeiten, einen String in Kleinbuchstaben umzuwandeln. Manche Programmierer bevorzugen zum Beispiel die Verwendung von Regular Expressions oder die Nutzung von speziellen Bibliotheken. Wichtig ist, dass die gewählte Methode zu den Anforderungen des Programms sowie den persönlichen Vorlieben des Programmierers passt.

## Siehe auch

- [Die transform() Funktion](https://en.cppreference.com/w/cpp/string/byte/transform)
- [Die tolower() Funktion](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [Stack Overflow - How to convert a string to lower case in C++](https://stackoverflow.com/questions/313970/how-to-convert-stdstring-to-lower-case)