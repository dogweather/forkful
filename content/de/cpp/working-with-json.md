---
title:                "C++: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

JSON, oder JavaScript Object Notation, ist ein populäres Datenformat, das in der Regel zur Übertragung von strukturierten Daten zwischen verschiedenen Anwendungen verwendet wird. Es ist einfach zu lesen und zu schreiben und wird aufgrund seines geringen Speicherplatzbedarfs und seiner flexiblen Struktur häufig für die Kommunikation zwischen Client- und Serveranwendungen eingesetzt.

## Wie man damit arbeitet

Um mit JSON in C++ zu arbeiten, gibt es verschiedene Bibliotheken und Funktionen, die verwendet werden können. Eine der beliebtesten und einfachsten Möglichkeiten ist die Verwendung der Bibliothek "json.hpp" von Niels Lohmann, die eine leicht verständliche und benutzerfreundliche API bietet.

Ein einfaches Beispiel, wie man einen JSON-Datensatz in C++ erstellt, sieht wie folgt aus:

```C++
#include <iostream>
#include "json.hpp"

// Ein Beispiel-Datensatz, der in JSON-Format gespeichert werden soll
nlohmann::json data = {
    {"name", "Max Mustermann"},
    {"age", 30},
    {"hobbies", {"music", "reading", "sports"}}
};

int main()
{
    // JSON-Datensatz ausgeben
    std::cout << data.dump() << std::endl;

    // Daten aus dem Datensatz auslesen
    std::string name = data["name"];
    int age = data["age"];
    std::string first_hobby = data["hobbies"][0];
}
```

Das obige Beispiel zeigt, wie man einen JSON-Datensatz in C++ erstellt, ausgibt und Daten daraus ausliest. Die Bibliothek "json.hpp" erleichtert dabei die Arbeit, da sie die komplexe Syntax von JSON in einfachere C++-Befehle übersetzt.

## Tiefer Einblick

Obwohl JSON ein einfaches und flexibles Datenformat ist, gibt es bestimmte Regeln und Einschränkungen, die bei der Verwendung beachtet werden müssen. Zum Beispiel muss der Datensatz immer mit einem Objekt beginnen, das in geschweifte Klammern gesetzt wird, und die Daten sollten als Schlüssel-Wert-Paare gespeichert werden.

Darüber hinaus gibt es auch Beschränkungen, was die Datentypen angeht, die in JSON unterstützt werden. Zum Beispiel können keine Funktionen oder Pointer gespeichert werden und die Verwendung von Unicode-Zeichen muss genau beachtet werden.

Eine genauere Betrachtung dieser Regeln und Einschränkungen kann von Vorteil sein, um sicherzustellen, dass der JSON-Datensatz korrekt aufgebaut ist und somit eine fehlerfreie Kommunikation zwischen verschiedenen Anwendungen ermöglicht.

## Siehe auch

- [json.hpp Bibliothek von Niels Lohmann](https://github.com/nlohmann/json)
- [JSON-Dokumentation auf der offiziellen Website](https://www.json.org/json-de.html)
- [Tutorial zur Verwendung von JSON in C++ von CodeProject](https://www.codeproject.com/Articles/869108/Getting-Started-with-JSON-in-Cplusplus)