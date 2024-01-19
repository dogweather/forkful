---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# C++ Zeichenketten durch ein Muster ersetzen

## Was & Warum?

Im Wesentlichen geht es beim Löschen von Zeichen, die einem Muster entsprechen, darum, unerwünschte Elemente aus einer Zeichenkette zu entfernen. Dies ist besonders nützlich, um den Code sauber und fehlerfrei zu halten.

## Anleitung:

Die C++ `<algorithm>` und `<string>` Bibliotheken zusammen machen diese Aufgabe relativ leicht. Im folgenden Beispiel ersetzen wir alle cvowels mit dem '-' Zeichen.

```C++
#include <algorithm>
#include <string>
#include <iostream>

int main() {
    std::string str = "Awesome C++ programming!";
    const std::string vowels = "aeiou";

    std::replace_if(str.begin(), str.end(), 
                    [&vowels](const char& c) {
                        return vowels.find(tolower(c)) != std::string::npos; 
                    }, 
                    '-');
                      
    std::cout << str << std::endl;
  
    return 0;
}
```
Dieser Code gibt folgendes aus: 

```
'-w-s-m- C++ pr-gr-mm-ng!'
```

## Vertiefung:

Historisch gesehen musste dies manuell mit Schleifen und bedingten Anweisungen erreicht werden, was zeitaufwändig und fehleranfällig war. Mit den `<algorithm>` und `<string>` Bibliotheken wird dieser Prozess jetzt erheblich vereinfacht.

Es gibt Alternativen zur Methode `.replace_if()`, z.B. die Nutzung von regulären Ausdrücken, die jedoch oft komplexer in ihrem Gebrauch sind. 

Was die Implementierungsdetails betrifft, so läuft `.replace_if()` durch die Zeichenkette und prüft jeden Charakter gegen das Muster. Wenn eine Übereinstimmung gefunden wird, ersetzt es das Zeichen. Seine Komplexität ist linear und abhängig von der Länge der Zeichenkette.

## Siehe Auch:

- [C++ Reference - replace_if](http://www.cplusplus.com/reference/algorithm/replace_if/)
- [StackOverflow - How to replace all letters in a string?](https://stackoverflow.com/questions/415432/how-to-replace-all-occurrences-of-characters-in-a-c-string)
- [C++ Reference - Reguläre Ausdrücke (RegEx)](http://www.cplusplus.com/reference/regex/)