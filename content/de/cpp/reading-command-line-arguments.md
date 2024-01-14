---
title:    "C++: Lesen von Befehlszeilenargumenten"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist ein wichtiger Bestandteil der C++ Programmierung. Es ermöglicht die Interaktion mit dem Programm durch die Eingabe von Daten während der Laufzeit. Dies ist besonders nützlich, wenn das Programm für verschiedene Zwecke verwendet werden kann und die Verwendung von festen Werten im Code vermieden werden soll. In diesem Blogpost erfahren Sie, wie Sie auf einfache Weise Befehlszeilenargumente in Ihrem C++ Programm lesen können.

## Wie man Befehlszeilenargumente liest

Das Lesen von Befehlszeilenargumenten in C++ ist relativ einfach und erfordert nur wenige Schritte. Zunächst müssen Sie die Hauptfunktion Ihres Programms deklarieren. In der Regel hat diese Funktion zwei Parameter: argc (argument count) und argv (argument vector). Diese Parameter werden vom Betriebssystem bereitgestellt und enthalten Informationen über die Eingabeaufforderung und die eingegebenen Argumente.

Als nächstes müssen Sie eine for-Schleife verwenden, um durch alle Argumente zu iterieren. Beginnen Sie bei Index 1, da der erste Wert in argv immer der Name des Programms ist. Innerhalb dieser Schleife können Sie dann auf die einzelnen Argumente zugreifen und sie entsprechend verarbeiten.

```C++
int main(int argc, char** argv) {

    // Iteriere durch alle Argumente
    for (int i = 1; i < argc; i++) {
        // Verarbeite das Argument an Index i
    }

    return 0;
}
```

Um ein Argument zu lesen, können Sie einfach auf den entsprechenden Index in argv zugreifen, z.B ```argv[i]```. Dieser Wert ist vom Typ ```char*``` und muss möglicherweise noch in den gewünschten Datentyp konvertiert werden.

Um die Argumente aus der Eingabeaufforderung zu überprüfen, können Sie Ihr Programm in der Kommandozeile ausführen und dabei verschiedene Argumente eingeben, z.B ```./meinprogramm argument1 argument2```. Die Ausgabe könnte dann wie folgt aussehen:

```
Das erste Argument ist: argument1
Das zweite Argument ist: argument2
```

## Tiefere Einblicke

Es gibt verschiedene Möglichkeiten, das Lesen von Befehlszeilenargumenten in C++ zu erweitern. Zum Beispiel können Sie die Bibliothek ```sstream``` verwenden, um verschiedene Datentypen aus den Argumenten zu lesen. Sie können auch benutzerdefinierte Fehlermeldungen erstellen, wenn bestimmte Argumente fehlen oder falsch formatiert sind.

Es ist auch wichtig zu beachten, dass Befehlszeilenargumente nicht nur für Eingabeelemente verwendet werden können, sondern auch für die Konfiguration von Programmen, z.B indem Sie Optionen oder Flags übergeben.

## Siehe auch

- [C++ Dokumentation: Kommandozeilenargumente](https://de.cppreference.com/w/cpp/language/main_function)
- [Tutorial: Befehlszeilenargumente in C++ lesen](https://www.learncpp.com/cpp-tutorial/reading-arguments-from-the-command-line/)
- [Erweiterte Möglichkeiten beim Lesen von Befehlszeilenargumenten in C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)