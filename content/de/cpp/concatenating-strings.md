---
title:                "C++: Zeichenfolgen verknüpfen"
simple_title:         "Zeichenfolgen verknüpfen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum?

Eine der grundlegendsten und häufigsten Aufgaben in der Programmierung ist die Manipulation von Strings. Das Zusammenfügen oder Concatenating von Strings ermöglicht es, mehrere Textteile zu einem Ganzen zusammenzusetzen. Dies kann in verschiedenen Anwendungsgebieten nützlich sein, wie zum Beispiel bei der Erstellung von dynamischen Ausgaben oder bei der Bearbeitung von Benutzereingaben.

## Wie geht das?

In C++ gibt es zwei Möglichkeiten, Strings miteinander zu verbinden: mit dem "+" Operator oder mit der Funktion "concat". Beide verwenden den Datentyp "string", um die Textstücke zu halten. Hier ist ein Beispiel für beide Methoden:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // "+" Operator
    string str1 = "Hello";
    string str2 = "World!";
    string result = str1 + " " + str2;
    cout << result << endl;
    
    // "concat" Funktion
    string text1 = "John";
    string text2 = "Doe";
    string fullName = text1.concat(" ").concat(text2);
    cout << fullName << endl;
    
    return 0;
}
```

Dieser Code wird die Strings "Hello World!" und "John Doe" ausgeben. Beachten Sie, dass sowohl der Operator als auch die Funktion die Strings zusammenfügen und eine neue verkettete Zeichenkette erstellen.

## Tiefere Einblicke

Beim Zusammenführen von Strings mit dem "+" Operator oder der "concat" Funktion ist es wichtig zu beachten, dass beide Funktionen den ursprünglichen String nicht verändern. Stattdessen wird eine neue Zeichenkette erstellt, die anschließend verwendet oder gespeichert werden kann. Dies hilft, unerwünschte Änderungen an Originaltexten zu vermeiden.

Außerdem können beide Methoden auch mehr als zwei Strings gleichzeitig zusammenfügen. Zum Beispiel können Sie mit dem "+" Operator mehrere Zeichenketten hintereinander anfügen, während Sie bei der "concat" Funktion eine beliebige Anzahl von Zeichenketten als Parameter übergeben können.

## Siehe auch

- [C++ Strings](https://www.cplusplus.com/reference/string/string/)
- [C++ String Concatenation](https://www.programiz.com/cpp-programming/library-function/cplusplus-string-concat)

Danke, dass Sie sich die Zeit genommen haben, diesen Beitrag über das Zusammenfügen von Strings in C++ zu lesen. Wir hoffen, dass Sie nun ein besseres Verständnis dafür haben, wie Sie Strings in Ihren Programmen effektiv verbinden können. Bis zum nächsten Mal!