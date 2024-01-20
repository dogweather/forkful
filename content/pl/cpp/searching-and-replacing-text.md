---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyszukiwanie i zastępowanie tekstu to podstawowy zabieg programistyczny, który pozwala na lokalizowanie określonego ciągu znaków (wyszukiwanie) i jego zamianę na inny (zastępowanie). Programiści robią to, aby szybko zmieniać kod, poprawiać błędy czy dostosowywać oprogramowanie do nowych wymagań.

## Jak to Zrobić:

Poniżej znajduje się przykładowy kod służący do wyszukiwania i zastępowania tekstu, napisany w języku C++:

```C++
#include <string>
using namespace std;

int main() {
    string tekst = "Jestem programistą C++";
    size_t pozycja = tekst.find("programistą");

    if (pozycja != string::npos) {
        tekst.replace(pozycja, 12, "mistrzem");
    }

   cout << tekst << endl;  //"Jestem mistrzem C++"

    return 0;
}
```
W powyższym kodzie, metoda `find()` wyszukuje "programistą" w tekście, a `replace()` zastępuje go słowem "mistrzem".

## W Głąb Tematu:

**Historycznie:** Wyszukiwanie i zastępowanie tekstu jest tak starym konceptem jak same początki programowania. Było niezbędne do optymalizacji przepływu danych i poprawy wydajności.

**Alternatywy:** Mamy różne sposoby na wyszukiwanie i zastępowanie tekstu. Możemy użyć standardowych bibliotek C++, takich jak `<algorithm>`. Możemy też korzystać z bibliotek zewnętrznych, takich jak Boost.

**Szczegóły Implementacji:** W naszym przypadku, `find()` zwraca pozycję początkową pierwszego wystąpienia danego ciągu. Jeżeli ciąg nie zostanie znaleziony, funkcja zwraca `string::npos`. Po znalezieniu pozycji, możemy użyć `replace()`, podając jej pozycję startową, długość ciągu do zastąpienia i nowy ciąg.

## Zobacz Również:

1. Dokumentację standardowej biblioteki szablonów C++ (STL): [STL Dokumentacja](http://www.cplusplus.com/reference/string/string/)
2. Dokumentacja biblioteki Boost: [Boost Dokumentacja](https://www.boost.org/doc/libs/)