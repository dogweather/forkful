---
title:                "Ekstrakcja podłańcuchów"
html_title:           "C++: Ekstrakcja podłańcuchów"
simple_title:         "Ekstrakcja podłańcuchów"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

Co i dlaczego?
Estrakcja podciągów to proces wydobywania fragmentów tekstu z większej całości. Programiści często korzystają z tej funkcji, aby wyodrębnić ważne informacje lub dokonać konkretnych operacji na tekście.

Jak to zrobić:
```C++
#include <iostream>
#include <string>

int main() {
    std::string text = "Przykładowy tekst do ekstrakcji";
    
    // Wyświetlenie całego tekstu
    std::cout << "Cały tekst: " << text << std::endl;
    
    // Ekstrakcja pojedynczego znaku
    char letter = text[6];
    std::cout << "Szósta litera to: " << letter << std::endl;
    
    // Ekstrakcja podciągu od pozycji 6 do 14
    std::string substring = text.substr(6, 8);
    std::cout << "Podciąg od 6 do 14: " << substring << std::endl;
    
    return 0;
}
```

Wyczerpujące informacje:
Estrakcja podciągów jest znana od dawna - pierwotnie była stosowana w języku FORTRAN w latach 60. Alternatywnym podejściem jest wykorzystanie wyrażeń regularnych. W implementacji podciągów w C++ wykorzystywana jest funkcja ```substr()```, który jako argumenty przyjmuje indeks początkowy i długość podciągu.

Zobacz też:
- https://en.cppreference.com/w/cpp/string/basic_string/substr
- https://www.geeksforgeeks.org/string-data-structure/
- https://www.geeksforgeeks.org/c-stringclasssubstr/