---
title:                "Wyświetlanie danych do debugowania"
html_title:           "C++: Wyświetlanie danych do debugowania"
simple_title:         "Wyświetlanie danych do debugowania"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

W drukowaniu wyjścia debugowania, programiści wykorzystują komunikaty w celu śledzenia i analizowania kodu. Jest to przydatne narzędzie do znajdowania błędów i poprawiania ich w trakcie pracy nad projektem.

## Jak to zrobić:

```C++
#include <iostream>
using namespace std;

int main() {
   int x = 5;
   cout << "Wartość zmiennej x to " << x << endl;
   return 0;
}
```

Po uruchomieniu kodu powyżej, wyświetlony zostanie napis "Wartość zmiennej x to 5".

## Głębszy wgląd:

W przeszłości, gdy komputery były wolniejsze i posiadały mniej pamięci, drukowanie wyjścia debugowania było nieefektywne. Dziś, dzięki zwiększonej mocy obliczeniowej i pamięci komputerów, jest to na ogół standardowe narzędzie programistyczne. Alternatywnie, programiści mogą używać narzędzi do debugowania, takich jak debugger, które pozwalają śledzić wywołania funkcji i wartości zmiennych w trakcie wykonywania kodu.

## Zobacz też:

Inne opcje debugowania w C++:
- [debugging tutorial](https://www.cplusplus.com/articles/1AqpX9Sz/) på stronie cplusplus.com
- [Visual Studio Code](https://code.visualstudio.com/docs/editor/debugging) - jedno z narzędzi do debugowania
- [gdb](https://www.gnu.org/software/gdb/) - konsolowe narzędzie do debugowania kodu