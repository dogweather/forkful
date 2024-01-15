---
title:                "Suchen und Ersetzen von Text"
html_title:           "C++: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Wenn du jemals versucht hast, große Mengen an Text zu bearbeiten, weißt du sicherlich, wie mühsam es sein kann, jedes einzelne Vorkommen eines bestimmten Wortes oder Ausdrucks zu ändern. Glücklicherweise ist C++ mit der Funktion zum Suchen und Ersetzen von Text ausgestattet, die es dir ermöglicht, diesen Prozess schnell und effizient durchzuführen.

## Wie geht's

Um Text in C++ zu suchen und zu ersetzen, musst du die <string> Bibliothek einbinden und die Funktion ```find()``` und ```replace()``` verwenden. Hier ist ein Beispiel, bei dem der Text "Hallo" durch "Tschüss" ersetzt wird:

```C++
#include <iostream>
#include <string>

int main() {
    std::string text = "Hallo Welt! Ich hoffe, es geht dir gut.";
    std::string oldWord = "Hallo";
    std::string newWord = "Tschüss";

    // Find position of oldWord in text
    size_t pos = text.find(oldWord);

    // Replace oldWord with newWord
    text.replace(pos, oldWord.length(), newWord);

    std::cout << text << std::endl;

    return 0;
}

// Output:
// Tschüss Welt! Ich hoffe, es geht dir gut.
```

Beachte, dass die Funktion ```find()``` die Position des gesuchten Wortes im Text zurückgibt. Diese Position wird dann in der Funktion ```replace()``` verwendet, um das Wort zu ersetzen.

## Tiefer Schritt

Die ```find()``` Funktion ist Teil der STL (Standard Template Library) in C++ und wird verwendet, um Unterzeichenketten in einem Text zu suchen. Sie gibt den Index der ersten Übereinstimmung oder ```std::string::npos``` zurück, wenn keine Übereinstimmung gefunden wurde. Die ```replace()``` Funktion nimmt die Position der zu ersetzenden Unterzeichenkette sowie deren Länge und den Ersatztext entgegen. Sie ersetzt einfach die angegebene Unterzeichenkette durch den Ersatztext, ohne den Rest des Textes zu beeinflussen.

In der Tiefe zu verstehen, wie diese Funktionen arbeiten, kann dir helfen, sie effektiv und effizient in deinem Code zu verwenden. Stelle sicher, dass du die Dokumentation der STL im Auge behältst, um alle Funktionen und ihre Verwendung zu verstehen.

## Siehe auch

- <https://www.cplusplus.com/reference/string/string/find/>
- <https://www.cplusplus.com/reference/string/string/replace/>
- <https://www.geeksforgeeks.org/find-and-replace-a-part-of-a-string-in-cpp-stl/>
- <https://www.tutorialspoint.com/cpp_standard_library/cpp_searching.htm>