---
title:    "C++: Großschreibung einer Zeichenkette"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum
Das Verändern der Groß- und Kleinschreibung eines Strings ist eine häufige Aufgabe in der C++ Programmierung. Dies kann aus ästhetischen Gründen geschehen, um die Lesbarkeit zu verbessern oder auch aus funktionalen Gründen, um bestimmte Bedingungen zu erfüllen. In diesem Blogpost werden wir uns darauf konzentrieren, wie man dies mit C++ erreichen kann.

## How To

### Beispiel 1

Um einen String in C++ zu capitalisieren, können wir die `toupper()` Funktion verwenden, die in der Header-Datei `<cctype>` enthalten ist. Diese Funktion nimmt einen Buchstaben als Argument und gibt die entsprechende Großbuchstabenversion zurück.

```C++
#include <iostream> 
#include <cctype> 
using namespace std; 

int main() 
{ 
    char c = 'h';
    cout << (char)toupper(c); 
    return 0; 
} 
```
Ausgabe: `H`

### Beispiel 2

Um einen gesamten String zu capitalisieren, können wir eine Schleife verwenden, die jeden Buchstaben des Strings nacheinander durchläuft und die `toupper()` Funktion darauf anwendet.

```C++
#include <iostream> 
#include <cctype> 
#include <string>
using namespace std; 

int main() 
{ 
    string str = "hallo welt!"; 
  
    for (int i = 0; i < str.length(); i++) { 
        str[i] = toupper(str[i]); 
    } 
  
    cout << str;
    return 0; 
} 
```
Ausgabe: `HALLO WELT!`

## Deep Dive

Bei der Verwendung der `toupper()` Funktion mit C++ ist es wichtig zu beachten, dass sie nur auf einzelne Zeichen angewendet werden kann und nicht auf ganze Strings. Deshalb müssen wir eine Schleife verwenden, um den Effekt auf den gesamten String anzuwenden.

Außerdem ist es wichtig zu verstehen, dass die `toupper()` Funktion das original Zeichen des Strings nicht ändert, sondern eine neue Kopie des Zeichens in Großbuchstaben zurückgibt. Deshalb müssen wir den Wert im String entsprechend aktualisieren.

Es gibt auch andere Möglichkeiten, einen String in C++ zu capitalisieren, zum Beispiel mit Hilfe von Bibliotheken oder vom Nutzer definierten Funktionen. Es ist wichtig, die verschiedenen Ansätze zu verstehen und zu wissen, wann und warum wir sie verwenden sollten.

## Siehe auch
- [Documentation - C++ toupper()](https://www.geeksforgeeks.org/toupper-function-in-c/)
- [CPPReference - toupper()](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [Codecademy - Capitalize a string in C++](https://www.codecademy.com/forum_questions/5239e3bd5923788287001126)