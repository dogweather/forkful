---
title:    "C++: Verbinden von Zeichenketten"
keywords: ["C++"]
---

{{< edit_this_page >}}

Warum: Jeder, der C++ programmiert, wird früher oder später auf die Notwendigkeit stoßen, Strings zu kombinieren. Dies kann z.B. bei der Ausgabe von Texten oder bei der Verarbeitung von Benutzereingaben nützlich sein.

Wie: Die Konkatenation von Strings in C++ ist relativ einfach und wird durch den "+" Operator durchgeführt. Um zwei Strings zu verbinden, müssen sie einfach hintereinander platziert werden, zum Beispiel: "Hallo" + "Welt". Innerhalb der "```C++ ... ```" Code Blöcke können wir sehen, wie dies in der Praxis aussieht:

```C++
#include <iostream>
using namespace std;

int main() {
  string erstes = "Hallo";
  string zweites = "Welt";
  string kombiniert = erstes + zweites;
  cout << kombiniert;
  return 0;
}
```

Die Ausgabe dieses Codes wäre "HalloWelt". Wir können auch mehrere Strings zu einem kombinieren, indem wir einfach den "+" Operator zwischen ihnen verwenden.

Tiefere Einblicke: Es gibt ein paar Dinge, die wir beachten müssen, wenn wir Strings in C++ kombinieren. Zum Beispiel müssen die Strings denselben Datentyp haben, sonst wird der Compiler einen Fehler ausgeben. Außerdem kann der "+" Operator auch mit anderen Datentypen verwendet werden, wie z.B. Integer oder Floats. In diesem Fall wird der Zahlenwert dem String einfach angehängt. Eine weitere wichtige Sache ist, dass die Zeichenlänge der kombinierten Strings nicht größer sein darf als die Größe des ursprünglichen Strings. Andernfalls wird es zu einer unerwarteten Ausgabe oder sogar einem Programmabsturz führen.

See Also:
- [C++ String Funktionen](https://de.cppreference.com/w/cpp/string/basic_string)
- [String Konvertierung in C++](https://www.tutorialspoint.com/cplusplus/cpp_strings_converting.htm)