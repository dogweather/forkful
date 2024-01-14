---
title:    "C++: Substrings extrahieren"
keywords: ["C++"]
---

{{< edit_this_page >}}

##Warum

Manchmal müssen wir in unseren Programmen einzelne Teile von Zeichenketten extrahieren, anstatt die gesamte Zeichenkette zu verwenden. Dies kann aus verschiedenen Gründen geschehen, wie beispielsweise die Überprüfung von Benutzereingaben oder die Formatierung von Daten. Wenn Sie sich fragen, wie Sie dies in C++ erreichen können, lesen Sie weiter!

##So geht's

Die Extraktion von Teilzeichenketten in C++ kann auf verschiedene Weise erfolgen, abhängig von Ihren spezifischen Anforderungen. Hier sind einige Beispiele, die Ihnen helfen können, die richtige Methode für Ihre Situation zu wählen:

```C++
// Beispiel 1: Extraktion einer bestimmten Anzahl von Zeichen von einer gegebenen Position
#include <iostream>
#include <string>

using namespace std;

int main() {
  string str = "Dies ist ein Beispiel";
  
  // Extrahiere 5 Zeichen, beginnend bei Indexposition 10
  string substr1 = str.substr(10, 5);
  
  cout << substr1 << endl; // Ausgabe: "ein B"
  
  return 0;
}

```

```C++
// Beispiel 2: Extraktion einer Teilzeichenkette bis zum Ende einer gegebenen Position
#include <iostream>
#include <string>

using namespace std;

int main() {
  string str = "Dies ist ein Beispiel";
  
  // Extrahiere Zeichen ab Indexposition 5 bis zum Ende
  string substr2 = str.substr(5);
  
  cout << substr2 << endl; // Ausgabe: "ist ein Beispiel"
  
  return 0;
}

```

```C++
// Beispiel 3: Extraktion einer Teilzeichenkette bis zum ersten Vorkommen einer bestimmten Zeichenfolge
#include <iostream>
#include <string>

using namespace std;

int main() {
  string str = "Dies ist ein Beispiel";
  
  // Extrahiere Zeichen bis zum ersten Auftreten von "ist"
  string substr3 = str.substr(0, str.find("ist"));
  
  cout << substr3 << endl; // Ausgabe: "Dies "
  
  return 0;
}

```

##Deep Dive

Die C++ `substr()`-Funktion erlaubt es uns, Teilzeichenketten aus einer gegebenen Zeichenkette zu extrahieren. Sie benötigt zwei Parameter: den Index der Zeichenkette, an dem der Extrakt beginnen soll, und die Anzahl der zu extrahierenden Zeichen. Sie können auch ein zweites Argument weglassen, um alle Zeichen ab dem gegebenen Index bis zum Ende zu extrahieren. Wenn Sie nach einer bestimmten Zeichenfolge suchen, können Sie die `find()`-Funktion verwenden, um die Position dieser Zeichenfolge zu erhalten und sie dann als Teil des ersten Parameters an `substr()` weitergeben.

##Siehe auch

- [C++ String.substr() Referenz](https://www.cplusplus.com/reference/string/string/substr/)
- [C++ String.find() Referenz](https://www.cplusplus.com/reference/string/string/find/)
- [C++ Strings Tutorial](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)