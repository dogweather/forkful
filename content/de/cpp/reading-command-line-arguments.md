---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "C++: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn du programmierst, möchtest du vielleicht deiner Anwendung ermöglichen, auf verschiedene Arten verwendet zu werden. Das Einlesen von Befehlszeilenargumenten ist eine gängige Methode, um dies zu erreichen.

## Wie es geht

Es gibt mehrere Möglichkeiten, Befehlszeilenargumente in C++ zu lesen. Hier sind zwei Beispiele:

1. **argc und argv:** Mit den Variablen `argc` und `argv` kannst du auf die Befehlszeilenargumente in deinem Programm zugreifen. `argc` enthält die Anzahl der Argumente und `argv` ist ein Array von Zeigern, die auf Strings mit den Argumenten verweisen.

```C++
#include <iostream>

int main(int argc, char* argv[]) {
  std::cout << "Anzahl der Argumente: " << argc << std::endl;
  std::cout << "Argumente:" << std::endl;
  for (int i = 0; i < argc; i++) {
    std::cout << argv[i] << std::endl;
  }
}
```

**Ausgabe:**

```
Anzahl der Argumente: 3
Argumente:
./program
arg1
arg2
```

2. **Boost.Program_options:** Diese Bibliothek bietet eine umfangreichere Möglichkeit, Befehlszeilenargumente zu lesen. Sie ermöglicht die Verwendung von Optionen, die mit oder ohne Werte verwendet werden können, gekennzeichnet mit `--` oder `-`.

```C++
#include <iostream>
#include <boost/program_options.hpp>

namespace po = boost::program_options;

int main(int argc, char* argv[]) {
  try {
    po::options_description desc{"Options"};
    desc.add_options()
      ("help,h", "Hilfe anzeigen")
      ("input,i", po::value<std::string>(), "Eingabedatei")
      ("output,o", po::value<std::string>(), "Ausgabedatei");

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, desc), vm);
    po::notify(vm);

    if (vm.count("help")) {
      std::cout << desc << std::endl;
    }
    else if (vm.count("input")) {
      std::cout << "Eingabedatei: " << vm["input"].as<std::string>() << std::endl;
    }
    else {
      std::cout << "Kein gültiges Argument eingegeben." << std::endl;
    }
  }
  catch (std::exception& e) {
    std::cerr << "Fehler: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}
```

**Ausgabe:**

```
Benutzung: ./program [OPTIONEN]
Options:
  -h [ --help ]         Hilfe anzeigen
  -i [ --input ] arg    Eingabedatei
  -o [ --output ] arg   Ausgabedatei
```

## Tiefer eintauchen

Das Einlesen von Befehlszeilenargumenten kann auch komplexer gestaltet werden, z.B. durch Parsing von Arguments mit Whitespaces oder durch das Erstellen eigener Datenstrukturen für Optionen. Es gibt auch verschiedene Bibliotheken, die noch mehr Funktionalität beim Lesen von Befehlszeilenargumenten bieten, wie z.B. `getopt` oder `Docopt`.

## Siehe auch

- [C++-Referenz](https://de.cppreference.com/w/) - Offizielle Referenz von C++
- [Boost.Program_options-Dokumentation](https://www.boost.org/doc/libs/1_66_0/doc/html/program_options.html) - Dokumentation für die Boost-Bibliothek