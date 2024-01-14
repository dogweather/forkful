---
title:                "C++: Die Verkettung von Zeichenfolgen"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Die Verkettung von Zeichenfolgen (auch bekannt als String Concatenation) ist ein grundlegender Bestandteil der Programmierung in C++. Sie ermöglicht es, mehrere Zeichenfolgen zu einer einzigen zusammenzufügen, was besonders nützlich ist, wenn man dynamische Ausgaben erzeugen möchte. In diesem Blog-Beitrag werden wir uns genauer ansehen, wie man Strings in C++ verkettet und warum es wichtig ist, diese Technik zu beherrschen.

## Wie geht man vor

Um Zeichenfolgen zu verketten, benötigt man den Operand ```+```, der auch für die Addition von numerischen Werten verwendet wird. Das liegt daran, dass Zeichenfolgen in C++ immer noch als Arrays von Zeichen behandelt werden. Im Folgenden wird ein Beispiel gezeigt, wie man zwei Zeichenfolgen verkettet:

```C++
#include <iostream>

int main() {

    // Zwei Zeichenfolgen werden erstellt
    std::string str1 = "Hallo";
    std::string str2 = "Welt";

    // Die Verkettung wird mit dem Operand + durchgeführt
    std::string result = str1 + str2;

    // Das Ergebnis wird ausgegeben
    std::cout << result;

    return 0;
}
```

Die Ausgabe dieses Codes lautet:

```
HalloWelt
```

Wie man sehen kann, werden die beiden Zeichenfolgen zu einer einzigen zusammengefügt. Man kann auch mehrere Strings gleichzeitig verketten, indem man einfach weitere Operanden hinzufügt.

## Tiefergehende Informationen

Es gibt verschiedene Techniken, um Strings in C++ zu verketten, je nachdem, was genau man erreichen möchte. Eine Möglichkeit ist die Verwendung der Funktion ```append()```, die es erlaubt, Strings an einen bereits vorhandenen String anzufügen.

Um aber wirklich zu verstehen, wie Strings in C++ verkettet werden, ist es wichtig, das Konzept von Pointern und der Speicheradressierung zu verstehen. Da Zeichenfolgen in C++ als Char-Arrays behandelt werden, bezieht sich jede einzelne Zeichenfolge auf eine bestimmte Speicheradresse. So kann man zum Beispiel auch Strings verkettet werden, indem man die Speicheradressen von zwei Zeichenfolgen miteinander verknüpft.

Um mehrere Strings effizient zu verketten, kann man auch die Funktion ```std::stringstream``` verwenden, die es erlaubt, verschiedene Datentypen zu Strings zu konvertieren und sie dann miteinander zu verketten.

Insgesamt ist das Verständnis der Speicheradressierung und der verschiedenen Verkettungsmethoden von Zeichenfolgen ein wichtiger Teil der C++ Programmierung und kann dabei helfen, effizientere und dynamischere Programme zu schreiben.

## Siehe auch

* [C++ String concatenation](https://www.geeksforgeeks.org/string-concatenation-c/)
* [String Concatenation with pointers](https://www.programiz.com/cpp-programming/pointers-strings)
* [stringstream in C++](https://www.geeksforgeeks.org/stringstream-c-applications/)