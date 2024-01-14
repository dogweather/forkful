---
title:    "C++: Eine Textdatei lesen"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Warum

Das Lesen einer Textdatei kann eine nützliche Fähigkeit in der C++ Programmierung sein. Es ermöglicht es dem Programmierer, externe Daten in sein Programm zu integrieren und sie zu verarbeiten. Lesen Sie weiter, um zu erfahren, wie Sie eine Textdatei in C++ lesen können.

## Wie man eine Textdatei in C++ liest

Um eine Textdatei in C++ zu lesen, müssen wir zuerst die Header-Datei "fstream" einbinden. Diese wird verwendet, um Datenströme zu verarbeiten. Als nächstes können wir eine Instanz der Klasse "ifstream" erstellen, die für das Lesen von Dateien verwendet wird. Diese Instanz wird dann mit dem Dateinamen der zu lesenden Textdatei initialisiert.

```C++
#include <fstream>

int main()
{
  std::ifstream file("example.txt"); //öffnet die Textdatei "example.txt"
}
```

Um nun die Daten aus der Datei zu lesen, können wir die Methode "getline()" verwenden. Diese liest eine einzelne Zeile aus der Datei und speichert sie in einem String. Wir können eine Schleife verwenden, um alle Zeilen in der Datei zu lesen und sie auf der Konsole auszugeben.

```C++
#include <iostream>
#include <fstream>

int main()
{
  std::ifstream file("example.txt"); //öffnet die Textdatei "example.txt"
  std::string line;

  while(getline(file, line)){
    std::cout << line << std::endl; //gibt jede Zeile der Datei aus
  }
}
```

Die Ausgabe des obigen Codes wäre folgende:

```
Dies ist Zeile 1 der Textdatei.
Dies ist Zeile 2 der Textdatei.
Dies ist Zeile 3 der Textdatei.
```

## Tiefergehende Informationen über das Lesen von Textdateien

Das Lesen von Textdateien in C++ kann komplexer werden, wenn wir bestimmte Datenformate oder Zeichenkodierungen berücksichtigen müssen. Glücklicherweise bietet C++ Funktionen und Methoden, mit denen wir diese Herausforderungen bewältigen können. Eine weitere nützliche Methode ist "getline()" mit einer optionalen dritten Parameter, der das Trennzeichen für jede Zeile festlegen kann.

Weitere Informationen und Beispiele zu diesen Funktionen finden Sie in der [offiziellen Dokumentation von C++](https://en.cppreference.com/w/cpp/io/basic_ifstream/getline) oder in [diesem Tutorial zum Lesen von Dateien in C++](https://www.programiz.com/cpp-programming/file-handling).

## Siehe auch

- [Wie man eine Textdatei in C++ schreibt](https://www.example.com) (link to another blog post)
- [Verwendung von Dateistreams in C++](https://www.example.com) (link to a tutorial)
- [Offizielle Dokumentation zu C++ Ein- und Ausgabe](https://www.example.com) (link to official documentation)