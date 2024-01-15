---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "C++: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Es gibt mehrere Gründe, warum man einen String in Kleinbuchstaben konvertieren möchte. Zum Beispiel, wenn man sicherstellen möchte, dass alle Buchstaben in einer Eingabe auf die gleiche Weise behandelt werden, oder wenn man Strings vergleichen möchte und dabei auf die Groß- und Kleinschreibung keine Rolle spielen soll.

## Wie geht man vor?

Die Konvertierung eines Strings in Kleinbuchstaben kann mit nur wenigen Zeilen Code erreicht werden. Schauen wir uns dazu ein Beispiel an:

```C++
#include <iostream>
#include <string>
#include <algorithm> 
using namespace std;

int main()
{
    // Eingabe-String 
    string input = "HaLLo WoRld";
    
    // Konvertierung in Kleinbuchstaben
    transform(input.begin(), input.end(), input.begin(), ::tolower);
    
    // Ausgabe 
    cout << input << endl;
    
    return 0;
}

```

**Ausgabe:**
```
hallo world
```

In diesem Beispiel wird die ```transform()``` Funktion aus der Standard Template Library (STL) verwendet. Sie akzeptiert drei Argumente: den Anfang eines Bereichs, das Ende des Bereichs und eine Funktion, die auf jedes Element im Bereich angewendet wird. In diesem Fall verwenden wir die Funktion ```::tolower``` aus der Header-Datei ```<algorithm>```, um jeden Buchstaben in Kleinbuchstaben zu konvertieren. Die Funktion ```::tolower``` erwartet ein Argument vom Datentyp ```int``` und gibt einen Wert vom selben Datentyp zurück, der den entsprechenden Kleinbuchstaben darstellt.

## Tiefergehende Infos

Es ist wichtig zu erwähnen, dass die Konvertierung in Kleinbuchstaben von den Konventionen einer bestimmten Sprache oder Region abhängen kann. Zum Beispiel kann die Konvertierung von ```ß``` in einen Kleinbuchstaben in der deutschen Sprache unvorhersehbar sein. In solchen Fällen können spezifische Funktionen oder Bibliotheken verwendet werden, um eine genaue und kulturspezifische Konvertierung sicherzustellen.

## Siehe auch

- [Referenz für die STL ```transform()``` Funktion](https://www.cplusplus.com/reference/algorithm/transform/)
- [Tutorial zur Nutzung der STL in C++](https://www.tutorialspoint.com/cplusplus/cpp_stl_tutorial.htm)
- [Weitere Infos zur Behandlung von Strings in C++](https://www.cprogramming.com/tutorial/string.html)