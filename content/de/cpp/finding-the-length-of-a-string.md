---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Länge eines Strings zu finden ist die Quantifizierung, wie viele Zeichen er enthält. Dies ist häufig nötig, um Speicherbereiche zu verwalten oder Durchläufe zu steuern, in denen jedes Zeichen verarbeitet wird.

## Wie machen wir das:

```C++
#include <iostream> 
#include <string> 
  
int main() 
{ 
    std::string str = "Hallo Welt!"; 
  
    // Benutze die Funktion "length"
    std::cout << "Die Länge des Strings ist: " << str.length(); 
  
    return 0; 
} 
```
Ausgabe:

```
Die Länge des Strings ist: 12
```
## Tiefgehende Einblicke

Die Methode, um die Länge eines Strings in C++ zu finden, hat sich seit der ersten Implementierung kaum geändert. Alternativ könnten Sie eine Schleife erstellen, die jedes Zeichen durchläuft, bis sie ein Nullzeichen (''\0'') findet, aber dies wäre ineffizient verglichen mit der eingebauten `length` oder `size` Methode. Das Finden der Länge eines Strings ist O(1), das bedeutet die Komplexität ist konstant, weil der Speicherort der Stringlänge in der Stringklasse selbst ist.

## Siehe auch
1. [C++ Stringfunctionen Referenz](http://www.cplusplus.com/reference/string/string/)