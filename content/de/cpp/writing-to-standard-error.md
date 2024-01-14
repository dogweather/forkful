---
title:    "C++: Schreiben auf Standardfehler"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf Standardfehler ist eine nützliche Technik, die häufig in der C++ Programmierung verwendet wird. Es ermöglicht die Ausgabe von Fehlermeldungen und anderen wichtigen Informationen während der Laufzeit des Programms. Dies kann besonders hilfreich sein, wenn das Programm mit verschiedenen Eingaben getestet wird oder in einer produktiven Umgebung ausgeführt wird.

## Wie gehe ich vor

Das Schreiben auf Standardfehler ist relativ einfach in C++ zu implementieren. Im Folgenden finden Sie ein Beispiel, das die Verwendung von "cerr" demonstriert:

```C++
#include <iostream>

int main() {
    std::cerr << "Dies ist eine Fehlermeldung." << std::endl;
    return 0;
}
```

Die Ausgabe dieses Codes wird so aussehen:

```
Dies ist eine Fehlermeldung.
```

Wie Sie sehen können, können wir ganz einfach eine Nachricht auf dem Standardfehler ausgeben, indem wir den "<

## Tiefergehende Informationen

In der obigen Beispiel können wir sehen, dass die Ausgabe auf dem Standardfehler mit dem "cerr" Objekt erfolgt. Dies ist ein Objekt vom Typ "ostream", der für die Ausgabe von Daten auf dem Standardfehler zuständig ist. Das "std::endl" am Ende ist dafür verantwortlich, dass die Ausgabe in einer neuen Zeile erfolgt.

Es ist auch wichtig zu beachten, dass der Standardfehler in einer separaten Datenstruktur gespeichert wird als der Standardausgang ("cout"). Dies ist hilfreich, da es ermöglicht, dass wichtige Fehlermeldungen von anderen Programmausgaben getrennt werden.

## Siehe auch

- [C++ Referenz für Standardfehler](https://en.cppreference.com/w/cpp/io/cerr)
- [C++ Dokumentation für Standardausgabe und Standardfehler](https://www.cplusplus.com/articles/iE86b7Xj/)