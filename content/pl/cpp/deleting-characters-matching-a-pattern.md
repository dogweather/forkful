---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Usuwanie znaków pasujących do wzorca jest procesem eliminowania określonych znaków z ciągu znaków w programowaniu. Programiści robią to, aby oczyścić i standardyzować dane wejściowe, usprawniając w ten sposób proces analizy danych.

## Jak to zrobić:

Podany poniżej kod demonstruje, jak usunąć wszystkie wystąpienia litery 'a' z danego ciągu znaków:
```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string str = "Kasia ma kota";
    str.erase(std::remove(str.begin(), str.end(), 'a'), str.end());
    std::cout << str << std::endl;
    return 0;
}
```
Po uruchomieniu programu, wydrukuje on "Ksi m kot".

## W głąb tematu:

Usuwanie znaków pasujących do wzorca jest często stosowanym narzędziem, szczególnie w przetwarzaniu tekstu. W szczególności, było szeroko używane w przeszłości, gdy rozbudowane systemy baz danych nie były jeszcze powszechne.

Istnieją różne metody usuwania znaków, w zależności od wymagań konkretnego projektu. Na przykład, można użyć funkcji `regex_replace` z biblioteki `std::regex`, aby usunąć wszystkie znaki pasujące do danego wzorca regularnego.

Założeniem funkcji `std::remove` jest przesunięcie znaków, które nie pasują do podanego argumentu, na początek ciągu, a następnie zwrócenie iteratora do pierwszego "usuniętego" elementu. Zastosowanie `erase` pozwala na bezpieczne usunięcie "usuniętych" znaków.

## Zobacz też:

1. Dokumentacja C++ na std::remove: http://www.cplusplus.com/reference/algorithm/remove/
2. Dokumentacja C++ na std::erase: https://en.cppreference.com/w/cpp/string/basic_string/erase
3. Dokumentacja C++ na std::regex_replace: http://www.cplusplus.com/reference/regex/regex_replace/