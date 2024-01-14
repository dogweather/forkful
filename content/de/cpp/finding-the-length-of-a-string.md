---
title:    "C++: Ermitteln der Länge eines Strings."
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Egal ob Sie ein Anfänger oder ein erfahrener C++ Programmierer sind, Sie werden früher oder später auf die Aufgabe stoßen, die Länge eines Strings zu bestimmen. Dies kann aus verschiedenen Gründen wichtig sein, sei es um den zugewiesenen Speicherplatz zu überprüfen oder um sicherzustellen, dass die korrekte Anzahl von Zeichen eingegeben wurde. In diesem Blogbeitrag werden wir uns daher ansehen, wie man die Länge eines Strings in C++ bestimmen kann.

## Wie geht man vor?

Die Länge eines Strings in C++ kann auf verschiedene Arten bestimmt werden, aber wir werden uns hier auf die Verwendung der Standardbibliothek konzentrieren. Um die Länge eines Strings zu bestimmen, wird die Funktion `length()` verwendet, die in der Header-Datei `<string>` definiert ist. Hier ist ein einfaches Beispiel, wie man die Länge eines Strings bestimmt:

```C++
#include <iostream> 
#include <string> 

using namespace std; 
  
int main() 
{ 
    string myString = "Guten Tag!"; 

    cout << "Die Länge des Strings ist: " << myString.length() << endl; 

    return 0; 
} 
```

**Ausgabe:**

Die Länge des Strings ist: 10

Wie Sie sehen können, verwenden wir die Methode `length()` auf dem String-Objekt `myString`, um die Länge des Strings zu erhalten. Dies ist die einfachste und effektivste Methode, um die Länge eines Strings in C++ zu bestimmen.

## Tieferes Eintauchen

Es ist wichtig zu verstehen, dass die Länge eines Strings in C++ auf zwei Arten bestimmt werden kann - entweder durch die Funktion `length()` oder durch die Funktion `size()`, die ebenfalls in der Header-Datei `<string>` enthalten ist. Beide Funktionen führen im Grunde das gleiche aus, jedoch gibt `length()` explizit die Anzahl der Zeichen zurück, während `size()` je nach Implementierung des Compilers auch die Anzahl der Bytes zurückgeben kann.

Wenn Sie tiefer in die Materie eintauchen möchten, können Sie sich auch mit der Null-Terminierung von Strings befassen, die in C++ sehr wichtig ist. Dies bedeutet, dass ein String mit einem Nullbyte enden muss, damit C++ weiß, wo der String endet. Das bedeutet auch, dass der Nullbyte nicht zur Länge des Strings gezählt wird.

## Siehe auch

- [C++ String-Bibliotheksdokumentation](https://en.cppreference.com/w/cpp/string/basic_string) 
- [Differenz zwischen `length()` und `size()`](https://stackoverflow.com/questions/380241/in-c-does-the-string-length-method-and-size-method-do-the-same-thing) 
- [Null-Terminierung von Strings](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)